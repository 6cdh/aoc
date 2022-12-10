function run_cpu()
    instructions = readlines()

    xs::Vector{Int} = []
    x = 1
    for ins in instructions
        ws = split(ins)
        if ws[1] == "noop"
            push!(xs, x)
        else
            push!(xs, x)
            push!(xs, x)
            x += parse(Int, ws[2])
        end
    end
    xs
end

function day10()
    xs = run_cpu()
    n = length(xs)

    println(sum(map(t -> t * xs[t], 20:40:n)))

    for i in 1:6
        for j in 1:40
            cycle = (i - 1) * 40 + j
            if xs[cycle] - 1 <= ((cycle - 1) % 40) <= xs[cycle] + 1
                print('#')
            else
                print('.')
            end
        end
        println()
    end
end

@time day10()
