-module(dolphins).
-compile(export_all).

dolphin1() ->
    receive
        do_a_flip -> io:format("How about no?~n");
        fish ->
            io:format("So long and thanks for all the fish!~n");
        _ -> io:format("Heh, we're smarter than you humans.~n")
    end.
