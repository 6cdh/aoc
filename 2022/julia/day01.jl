function day01()
    lines = readlines()
    calories::Vector{Int} = [0]

    for line in lines
        if isempty(line)
            push!(calories, 0)
        else
            calories[length(calories)] += parse(Int, line)
        end
    end
    sort!(calories, rev=true)

    println(calories[1])
    println(sum(calories[1:3]))
end

@time day01()
