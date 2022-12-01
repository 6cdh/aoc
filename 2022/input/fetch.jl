using HTTP
using Printf

function download()
    day = parse(Int, ARGS[1])
    cookie = readline("cookie")
    res = HTTP.get(
        "https://adventofcode.com/2022/day/$day/input",
        ["Cookie" => cookie]
    )

    file = @sprintf("day%.2d.txt", day)

    if res.status == 200
        input = String(res.body)
        write(file, input)
        println(
            "fetched $file with ",
            count(x -> x == '\n', input),
            " lines")
    else
        println("Error: status $(res.status)")
    end
end

download()
