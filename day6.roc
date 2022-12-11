app "day6"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br" }
    imports [pf.Stdout, pf.File, pf.Path, pf.Task]
    provides [main] to pf

findStarter : Str, Nat -> U32
findStarter = \input, requiredLen ->
    (
        input
        |> Str.toUtf8
        |> List.walkUntil { buf: [], index: 0 } \{ buf, index }, char ->
            newState = {
                buf: (
                    if List.len buf >= requiredLen then
                        buf
                        |> List.dropFirst
                    else
                        buf
                )
                |> List.append char,
                index: index + 1,
            }
            if List.len newState.buf >= requiredLen then
                if (newState.buf |> Set.fromList |> Set.len) >= requiredLen then
                    Break newState
                else
                    Continue newState
            else
                Continue newState
    ).index

part1 = \input -> findStarter input 4
expect part1 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" == 7
expect part1 "bvwbjplbgvbhsrlpgdmjqwftvncz" == 5
expect part1 "nppdvjthqldpwncqszvftbrmjlhg" == 6
expect part1 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" == 10
expect part1 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" == 11

part2 = \input -> findStarter input 14
expect part2 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" == 19
expect part2 "bvwbjplbgvbhsrlpgdmjqwftvncz" == 23
expect part2 "nppdvjthqldpwncqszvftbrmjlhg" == 23
expect part2 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" == 29
expect part2 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" == 26

main =
    result <- Task.attempt (File.readUtf8 (Path.fromStr "./day6.txt"))
    count = Result.map result \file -> { first: part1 file, second: part2 file }
    when count is
        Ok { first, second } ->
            _ <- first |> Num.toStr |> Stdout.line |> Task.await
            second |> Num.toStr |> Stdout.line
        Err e -> when e is
            FileReadUtf8Err _path _ -> Stdout.line "Bad path"
            FileReadErr _path readError -> readError |> File.readErrToStr |> Stdout.line


