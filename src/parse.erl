-module(parse).

-export([file/1]).

% parse:file("C:/projects/git/cpro/p2a_sms_tariff.raml").
file(File) ->
    [map(Raml) || Raml <- yamerl_constr:file(File)].

map(Raml) ->
    lists:foldl(
      fun({"version",   V}, M) -> M#{version    => V};
         ({"securedBy", V}, M) -> M#{securedBy  => V};
         ({"title",     V}, M) -> M#{title      => list_to_binary(V)};
         ({"baseUri",   V}, M) -> M#{baseUri    => list_to_binary(V)};
         ({"mediaType", V}, M) -> M#{mediaType  => list_to_binary(V)};
         ({"schema", V}, M)    -> M#{schema  => list_to_binary(V)};
         ({"description", V}, M) -> M#{description  => list_to_binary(V)};
         ({"type", V}, M) -> M#{type  => list_to_binary(V)};
         ({"pattern", V}, M) -> M#{pattern  => list_to_binary(V)};
         ({"required", V}, M) when V == true; V == false -> M#{required  => V};
         ({"repeat", V}, M) when V == true; V == false -> M#{repeat  => V};
         ({"example", V}, M) ->
              M#{example =>
                 case catch jsx:decode(list_to_binary(V), [return_maps]) of
                  {'EXIT', _} when is_list(V) -> list_to_binary(V);
                  {'EXIT', _} -> V;
                     J -> J
                 end};
         ({"protocols", Protocols}, M) ->
              M#{protocols => [case VI of
                                   "HTTP" -> http;
                                   "HTTPS" -> https
                               end || VI <- Protocols]};
         ({"schemas", Schemas}, M) ->
              M#{schemas =>
                 [lists:foldl(
                   fun({K,S}, M1) ->
                           M1#{list_to_binary(K) =>
                               jsx:decode(list_to_binary(S), [return_maps])}
                   end, #{}, Schema) || Schema <- Schemas]};
         ({[$/|_] = K, V}, M) -> M#{list_to_binary(K) => map(V)};
         ({"get", V}, M) -> M#{get  => map(V)};
         ({"put", V}, M) -> M#{put  => map(V)};
         ({"post", V}, M) -> M#{post  => map(V)};
         ({"delete", V}, M) -> M#{delete  => map(V)};
         ({"body", V}, M) -> M#{body  => map(V)};
         ({"application/json", V}, M) -> M#{<<"application/json">> => map(V)};
         ({"uriParameters", V}, M) -> M#{uriParameters  => map(V)};
         ({"responses", V}, M) -> M#{responses  => map(V)};
         ({K, V}, M) when is_integer(K) -> M#{K  => map(V)};
         ({K, [{_,_}|_] = V}, M) when is_list(K) -> M#{list_to_binary(K)  => map(V)};
         (UP, M) ->
              io:format("** ~p~n", [UP]),
              M
      end, #{}, Raml).

