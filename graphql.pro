:- use_module(library(apply)).
:- use_module(library(http/json)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(aggregate), [foreach/2]).
:- use_module(library(lists), [member/2, subtract/3]).

% Predefined GraphQL endpoints for the AniList and Rick and Morty APIs
graphql_endpoint(anime_list, 'https://graphql.anilist.co/').
graphql_endpoint(rick_and_morty, 'https://rickandmortyapi.com/graphql').

assert_schema(EndpointId) :-
    retract_schema(EndpointId),
    graphql_endpoint(EndpointId, URL),
    introspect_schema(URL, Schema),
    assertz(schema_record(EndpointId, query_type(Schema.__schema.queryType.name))),
    assert_schema_types(EndpointId, Schema).

assert_schema_types(EndpointId, Schema) :-
    assert_schema_types_(EndpointId, Schema.__schema.types).

assert_schema_types_(_, []).
assert_schema_types_(EndpointId, [Type | Types]) :-
    assertz(schema_record(EndpointId, type(Type.name, Type.kind))),
    (
        Type.kind = "OBJECT" -> assert_schema_fields(EndpointId, Type.name, Type.fields);
        Type.kind = "INPUT_OBJECT" -> assert_schema_fields(EndpointId, Type.name, Type.inputFields);
        Type.kind = "ENUM" -> assert_schema_enum_values(EndpointId, Type.name, Type.enumValues);
        Type.kind = "SCALAR" -> true
    ),
    assert_schema_types_(EndpointId, Types).

assert_schema_fields(_, _, @(null)).
assert_schema_fields(_, _, []).
assert_schema_fields(EndpointId, TypeName, [Field | Fields]) :-
    assertz(schema_record(EndpointId, field(TypeName, Field.name, Field.type.name))),
    assert_schema_fields(EndpointId, TypeName, Fields).

assert_schema_enum_values(_, _, @(null)).
assert_schema_enum_values(_, _, []).
assert_schema_enum_values(EndpointId, TypeName, [EnumValue | EnumValues]) :-
    assertz(schema_record(EndpointId, enum_value(TypeName, EnumValue.name))),
    assert_schema_enum_values(EndpointId, TypeName, EnumValues).

retract_schema(EndpointId) :-
    findall(_, retract(schema_record(EndpointId, _)), _).

% Runs an introspection query on the specified GraphQL endpoint and returns the schema as a dict
% with the toplevel field being __schema
% Example usage:
%
% graphql_endpoint(anime_list, Endpoint), introspect_schema(Endpoint, Schema).

introspect_schema(URL, Schema) :-
    introspection_query_payload(Payload),
    http_post(URL, json(Payload), ResponseData, [request_header('Content-Type': 'application/json'), json_object(dict)]),
    Schema = ResponseData.data.

introspection_query_payload(Payload) :-
    introspection_graphql_query(Query),
    Payload = _{query: Query}.

introspection_graphql_query('
    query IntrospectionQuery {
    __schema {
        queryType {
          name
        }
        mutationType {
          name
        }
        subscriptionType {
          name
        }
        types {
          ...FullType
        }
        directives {
          name
          description
          locations
          args {
            ...InputValue
          }
        }
      }
    }

    fragment FullType on __Type {
        kind
        name
        description
        fields(includeDeprecated: true) {
            name
            description
            args {
            ...InputValue
            }
            type {
            ...TypeRef
            }
            isDeprecated
            deprecationReason
        }
        inputFields {
            ...InputValue
        }
        interfaces {
            ...TypeRef
        }
        enumValues(includeDeprecated: true) {
            name
            description
            isDeprecated
            deprecationReason
        }
        possibleTypes {
            ...TypeRef
        }
    }

    fragment InputValue on __InputValue {
        name
        description
        type {
            ...TypeRef
        }
        defaultValue
    }

    fragment TypeRef on __Type {
        kind
        name
        ofType {
            kind
            name
            ofType {
                kind
                name
                ofType {
                    kind
                    name
                    ofType {
                        kind
                        name
                        ofType {
                          kind
                          name
                          ofType {
                            kind
                            name
                            ofType {
                                kind
                                name
                            }
                         }
                     }
                 }
             }
          }
      }
    }'
).


% work with the database

schema_loaded(Id) :-
    schema_record(Id, _), !.

schema_loaded(Id) :-
    write('Schema with ID '), write(Id), write(' not loaded. Load it via assert_schema/1'), nl, fail.

query_type(EndpointId, QueryType) :-
    schema_loaded(EndpointId),
    schema_record(EndpointId, query_type(QueryType)).

object_type(EndpointId, TypeName) :-
    schema_loaded(EndpointId),
    schema_record(EndpointId, type(TypeName, "OBJECT")).

input_object_type(EndpointId, TypeName) :-
    schema_loaded(EndpointId),
    schema_record(EndpointId, type(TypeName, "INPUT_OBJECT")).

scalar_type(EndpointId, TypeName) :-
    schema_loaded(EndpointId),
    schema_record(EndpointId, type(TypeName, "SCALAR")).

enum_type(EndpointId, TypeName) :-
    schema_loaded(EndpointId),
    schema_record(EndpointId, type(TypeName, "ENUM")).

% Schema analytics

% Find all types in the given graph with the option to filter by kind
alltypes(EndpointId, Types) :- alltypes(EndpointId, _, Types).
alltypes(EndpointId, Kind, Types) :-
    schema_loaded(EndpointId),
    setof(X, Kind^schema_record(EndpointId, type(X, Kind)), Types).

path(EndpointId, FromType, ToType, Kind, _) :-
    schema_record(EndpointId, type(FromType, Kind)),
    schema_record(EndpointId, field(ToType, _, FromType)).

path(EndpointId, FromType, ToType, Kind, Visited) :-
    schema_record(EndpointId, field(FromType, _, Intermediate)),
    schema_record(EndpointId, type(Intermediate, Kind)),
    not(member(Intermediate, Visited)),
    path(EndpointId, Intermediate, ToType, Kind, [Intermediate | Visited]).

% Find all reachable types from a given type
% Optionally specify the kind of types to search
reachable(EndpointId, FromType, ReachableTypes) :- reachable(EndpointId, FromType, _, ReachableTypes).
reachable(EndpointId, FromType, Kind, ReachableTypes) :-
    schema_loaded(EndpointId),
    setof(X, Kind^path(EndpointId, FromType, X, Kind, [FromType]), ReachableTypes).

% Find all orphan types in the given graph
% Optionally specify the kind of types to search
orphans(EndpointId, OrphanTypes) :- orphans(EndpointId, _, OrphanTypes).
orphans(EndpointId, Kind, OrphanTypes) :-
    alltypes(EndpointId, Kind, AllTypes),
    query_type(EndpointId, QueryType),
    reachable(EndpointId, QueryType, Kind, ReachableTypes),
    subtract(AllTypes, ReachableTypes, OrphanTypes).
