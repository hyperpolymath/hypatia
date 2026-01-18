%% SPDX-License-Identifier: PLMP-1.0-or-later
%% cicd-hyper-a HTTP Client
%%
%% HTTP client wrapper for ArangoDB REST API interactions.
%% Uses SWI-Prolog's http library for HTTP operations.

:- object(http_client).

    :- info([
        version is 1:'0':'0',
        author is 'cicd-hyper-a',
        date is 2026-01-18,
        comment is 'HTTP client wrapper for ArangoDB REST API'
    ]).

    :- public([
        %% Core HTTP operations
        post_json/3,
        post_json/4,
        get_json/2,
        get_json/3,
        put_json/3,
        put_json/4,
        patch_json/3,
        patch_json/4,
        delete/2,
        delete/3,

        %% Connection management
        set_base_url/1,
        get_base_url/1,
        set_auth/2,
        clear_auth/0,

        %% Response handling
        parse_response/2,
        is_success/1,
        get_error_message/2,

        %% Request building
        build_url/2,
        build_url/3,
        add_query_params/3
    ]).

    :- private([
        base_url/1,
        auth_header/1,
        make_request/5,
        default_options/1
    ]).

    :- dynamic([
        base_url/1,
        auth_header/1
    ]).

    %% ============================================================
    %% CONNECTION MANAGEMENT
    %% ============================================================

    %% Set the base URL for all requests
    set_base_url(Url) :-
        retractall(base_url(_)),
        assertz(base_url(Url)).

    %% Get the current base URL
    get_base_url(Url) :-
        ( base_url(Url) -> true ; Url = 'http://localhost:8529' ).

    %% Set authentication credentials (Basic auth)
    set_auth(Username, Password) :-
        retractall(auth_header(_)),
        format(atom(Credentials), '~w:~w', [Username, Password]),
        base64(Credentials, Encoded),
        format(atom(Header), 'Basic ~w', [Encoded]),
        assertz(auth_header(Header)).

    %% Clear authentication
    clear_auth :-
        retractall(auth_header(_)).

    %% ============================================================
    %% CORE HTTP OPERATIONS
    %% ============================================================

    %% POST JSON data to endpoint
    %% post_json(+Endpoint, +JsonData, -Response)
    post_json(Endpoint, JsonData, Response) :-
        post_json(Endpoint, JsonData, [], Response).

    %% POST JSON with additional options
    %% post_json(+Endpoint, +JsonData, +Options, -Response)
    post_json(Endpoint, JsonData, Options, Response) :-
        make_request(post, Endpoint, JsonData, Options, Response).

    %% GET JSON from endpoint
    %% get_json(+Endpoint, -Response)
    get_json(Endpoint, Response) :-
        get_json(Endpoint, [], Response).

    %% GET JSON with options
    %% get_json(+Endpoint, +Options, -Response)
    get_json(Endpoint, Options, Response) :-
        make_request(get, Endpoint, none, Options, Response).

    %% PUT JSON to endpoint
    %% put_json(+Endpoint, +JsonData, -Response)
    put_json(Endpoint, JsonData, Response) :-
        put_json(Endpoint, JsonData, [], Response).

    %% PUT JSON with options
    %% put_json(+Endpoint, +JsonData, +Options, -Response)
    put_json(Endpoint, JsonData, Options, Response) :-
        make_request(put, Endpoint, JsonData, Options, Response).

    %% PATCH JSON to endpoint
    %% patch_json(+Endpoint, +JsonData, -Response)
    patch_json(Endpoint, JsonData, Response) :-
        patch_json(Endpoint, JsonData, [], Response).

    %% PATCH JSON with options
    %% patch_json(+Endpoint, +JsonData, +Options, -Response)
    patch_json(Endpoint, JsonData, Options, Response) :-
        make_request(patch, Endpoint, JsonData, Options, Response).

    %% DELETE endpoint
    %% delete(+Endpoint, -Response)
    delete(Endpoint, Response) :-
        delete(Endpoint, [], Response).

    %% DELETE with options
    %% delete(+Endpoint, +Options, -Response)
    delete(Endpoint, Options, Response) :-
        make_request(delete, Endpoint, none, Options, Response).

    %% ============================================================
    %% REQUEST BUILDING
    %% ============================================================

    %% Build full URL from endpoint
    build_url(Endpoint, Url) :-
        get_base_url(Base),
        ( atom_concat('/', _, Endpoint) ->
            atom_concat(Base, Endpoint, Url)
        ; format(atom(Url), '~w/~w', [Base, Endpoint])
        ).

    %% Build URL with path segments
    build_url(Endpoint, PathSegments, Url) :-
        get_base_url(Base),
        atomic_list_concat(PathSegments, '/', PathPart),
        ( atom_concat('/', _, Endpoint) ->
            format(atom(Url), '~w~w/~w', [Base, Endpoint, PathPart])
        ; format(atom(Url), '~w/~w/~w', [Base, Endpoint, PathPart])
        ).

    %% Add query parameters to URL
    add_query_params(Url, [], Url) :- !.
    add_query_params(Url, Params, UrlWithParams) :-
        findall(ParamStr,
            (member(Key=Value, Params),
             format(atom(ParamStr), '~w=~w', [Key, Value])),
            ParamStrs),
        atomic_list_concat(ParamStrs, '&', QueryStr),
        format(atom(UrlWithParams), '~w?~w', [Url, QueryStr]).

    %% ============================================================
    %% RESPONSE HANDLING
    %% ============================================================

    %% Parse HTTP response
    parse_response(RawResponse, ParsedResponse) :-
        ( is_dict(RawResponse) ->
            ParsedResponse = RawResponse
        ; atom(RawResponse) ->
            catch(
                ( open_string(RawResponse, Stream),
                  json_read_dict(Stream, ParsedResponse),
                  close(Stream) ),
                _,
                ParsedResponse = error(parse_failed, RawResponse)
            )
        ; string(RawResponse) ->
            catch(
                ( open_string(RawResponse, Stream),
                  json_read_dict(Stream, ParsedResponse),
                  close(Stream) ),
                _,
                ParsedResponse = error(parse_failed, RawResponse)
            )
        ; ParsedResponse = RawResponse
        ).

    %% Check if response indicates success
    is_success(Response) :-
        is_dict(Response),
        ( get_dict(error, Response, false)
        ; \+ get_dict(error, Response, _)
        ),
        \+ get_dict(errorNum, Response, _).

    is_success(response(Status, _Body)) :-
        Status >= 200,
        Status < 300.

    %% Get error message from response
    get_error_message(Response, Message) :-
        is_dict(Response),
        ( get_dict(errorMessage, Response, Message) -> true
        ; get_dict(error, Response, Message) -> true
        ; Message = 'Unknown error'
        ).

    get_error_message(response(Status, Body), Message) :-
        ( is_dict(Body), get_dict(errorMessage, Body, Msg) ->
            format(atom(Message), 'HTTP ~w: ~w', [Status, Msg])
        ; format(atom(Message), 'HTTP ~w', [Status])
        ).

    %% ============================================================
    %% INTERNAL PREDICATES
    %% ============================================================

    %% Default HTTP options
    default_options([
        timeout(30),
        request_header('Content-Type' = 'application/json'),
        request_header('Accept' = 'application/json')
    ]).

    %% Make HTTP request
    %% make_request(+Method, +Endpoint, +Body, +ExtraOptions, -Response)
    make_request(Method, Endpoint, Body, ExtraOptions, Response) :-
        build_url(Endpoint, Url),
        default_options(DefaultOpts),
        get_auth_options(AuthOpts),
        append([DefaultOpts, AuthOpts, ExtraOptions], AllOptions),
        build_request_body(Body, BodyOption),
        ( BodyOption \= [] ->
            append(AllOptions, BodyOption, FinalOptions)
        ; FinalOptions = AllOptions
        ),
        catch(
            execute_http_request(Method, Url, FinalOptions, Response),
            Error,
            Response = error(http_error, Error)
        ).

    %% Get authentication options
    get_auth_options(Options) :-
        ( auth_header(Header) ->
            Options = [request_header('Authorization' = Header)]
        ; Options = []
        ).

    %% Build request body option
    build_request_body(none, []) :- !.
    build_request_body(Body, [post(json(Body))]) :-
        is_dict(Body), !.
    build_request_body(Body, [post(json(Dict))]) :-
        is_list(Body),
        dict_create(Dict, json, Body), !.
    build_request_body(Body, [post(atom(Body))]) :-
        atom(Body), !.
    build_request_body(Body, [post(string(Body))]) :-
        string(Body).

    %% Execute the actual HTTP request using SWI-Prolog http library
    execute_http_request(get, Url, Options, Response) :-
        {http_open(Url, Stream, Options)},
        {json_read_dict(Stream, Response)},
        {close(Stream)}.

    execute_http_request(post, Url, Options, Response) :-
        extract_post_data(Options, Data, CleanOptions),
        {http_open(Url, Stream, [method(post), post(Data)|CleanOptions])},
        {json_read_dict(Stream, Response)},
        {close(Stream)}.

    execute_http_request(put, Url, Options, Response) :-
        extract_post_data(Options, Data, CleanOptions),
        {http_open(Url, Stream, [method(put), post(Data)|CleanOptions])},
        {json_read_dict(Stream, Response)},
        {close(Stream)}.

    execute_http_request(patch, Url, Options, Response) :-
        extract_post_data(Options, Data, CleanOptions),
        {http_open(Url, Stream, [method(patch), post(Data)|CleanOptions])},
        {json_read_dict(Stream, Response)},
        {close(Stream)}.

    execute_http_request(delete, Url, Options, Response) :-
        {http_open(Url, Stream, [method(delete)|Options])},
        {json_read_dict(Stream, Response)},
        {close(Stream)}.

    %% Extract post data from options
    extract_post_data(Options, Data, CleanOptions) :-
        ( member(post(Data), Options) ->
            delete(Options, post(_), CleanOptions)
        ; Data = '', CleanOptions = Options
        ).

    %% Base64 encoding (using SWI-Prolog built-in)
    :- if(\+ predicate_property(base64(_,_), built_in)).
    base64(Plain, Encoded) :-
        atom_codes(Plain, PlainCodes),
        {base64:base64(Encoded, PlainCodes)}.
    :- endif.

:- end_object.

%% ============================================================
%% PROLOG MODULE INTERFACE
%% Uses SWI-Prolog's http and json libraries
%% ============================================================

:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
