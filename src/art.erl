-module(art).
-compile(export_all).
-include_lib("stdlib/include/qlc.hrl"). 
 
-record(painting, {index, artist, title}).
  
init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(painting,
        [ {disc_copies, [node()] },
             {attributes,      
                record_info(fields,painting)} ]).
 
insert( Index, Artist, Title) ->
    Fun = fun() ->
         mnesia:write(
         #painting{ index=Index,
                    artist=Artist, 
                    title=Title } )
               end,
         mnesia:transaction(Fun).
  
select( Index) ->
    Fun = 
        fun() ->
            mnesia:read({painting, Index})
        end,
    {atomic, [Row]}=mnesia:transaction(Fun),
    io:format(" ~p ~p ~n ", [Row#painting.artist, Row#painting.title] ).

select_some( Artist) ->
    Fun = 
        fun() ->
            mnesia:match_object({painting, '_', Artist, '_' } )
        end,
    {atomic, Results} = mnesia:transaction( Fun),
    Results.
 
select_all() -> 
    mnesia:transaction( 
    fun() ->
        qlc:eval( qlc:q(
            [ X || X <- mnesia:table(painting) ] 
        )) 
    end ).

select_search( Word ) -> 
    mnesia:transaction( 
    fun() ->
         qlc:eval( qlc:q(
              [ {F0,F1,F2,F3} || 
                   {F0,F1,F2,F3} <- 
                        mnesia:table(painting),
                        (string:str(F2, Word)>0) or  
                        (string:str(F3, Word)>0)
               ] )) 
    end ).

