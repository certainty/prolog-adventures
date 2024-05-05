:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).

graphql_endpoint(anime_list, 'https://graphql.anilist.co/').
graphql_endpoint(rick_and_morty, 'https://rickandmortyapi.com/graphql').

introspect_schema(URL, Schema) :-
    introspection_query_payload(Payload),
    http_post(URL, json(Payload), ResponseData, [request_header('Content-Type': 'application/json'), json_object(dict)]),
    Schema = ResponseData.


introspection_query_payload(Payload) :-
    introspection_graphql_query(Query),
    Payload = _{query: Query}.

introspection_graphql_query2('query IntrospectionQuery { __schema  { queryType { name } } }').

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
