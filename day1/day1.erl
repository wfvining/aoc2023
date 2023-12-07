-module(day1).

-export([find_values/1]).
-compile(export_all).

re() ->
    {ok, RE} = re:compile("^[[:alpha:]]*(?|(\\d).*(\\d)|(\\d))[[:alpha:]]*$"),
    RE.

find_values(Path) ->
    RE = re(),
    {ok, File} = file:open(Path, [read]),
    calibration_values(RE, File, []).

calibration_values(RE, File, Values) ->
    case file:read_line(File) of
        {ok, "\n"} ->
            lists:reverse(Values);
        {ok, Data} ->
            Str = replace_words(Data),
            case re:run(Str, RE, [{capture, [1, 2], list}]) of
                {match, [X, []]} ->
                    calibration_values(RE, File, [list_to_integer(X ++ X) | Values]);
                {match, [X, Y]} ->
                    calibration_values(RE, File, [list_to_integer(X ++ Y) | Values]);
                nomatch ->
                    io:format("~p~n", [Data])
            end;
        eof ->
            lists:reverse(Values)
    end.

replace_words("one" ++ Rest)   -> "1" ++ replace_words("ne" ++ Rest);
replace_words("two" ++ Rest)   -> "2" ++ replace_words("wo" ++ Rest);
replace_words("three" ++ Rest) -> "3" ++ replace_words("hree" ++ Rest);
replace_words("four" ++ Rest)  -> "4" ++ replace_words("our" ++ Rest);
replace_words("five" ++ Rest)  -> "5" ++ replace_words("ive" ++ Rest);
replace_words("six" ++ Rest)   -> "6" ++ replace_words("ix" ++ Rest);
replace_words("seven" ++ Rest) -> "7" ++ replace_words("even" ++ Rest);
replace_words("eight" ++ Rest) -> "8" ++ replace_words("ight" ++ Rest);
replace_words("nine" ++ Rest)  -> "9" ++ replace_words("ine" ++ Rest);
replace_words([X|Rest])        -> [X|replace_words(Rest)];
replace_words([])              -> [].
