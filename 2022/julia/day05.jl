function rearrange(stacks, procedure; pack=false)
    for p in procedure
        cnt, from, to = p
        for _ in 1:cnt
            crate = pop!(stacks[from])
            push!(stacks[to], crate)
        end
        if pack
            n = length(stacks[to])
            reverse!(stacks[to], n - cnt + 1, n)
        end
    end

    str::Vector{Char} = []
    for i in 1:length(stacks)
        push!(str, last(stacks[i]))
    end
    println(join(str))
end

function day05()
    lines = readlines()

    stacks::Dict{Int,Vector{Char}} = Dict()

    i = 1
    while !isempty(lines[i])
        for j in 2:4:length(lines[i])
            if isuppercase(lines[i][j])
                index = j ÷ 4 + 1
                get!(stacks, index, [])
                push!(stacks[index], lines[i][j])
            end
        end
        i += 1
    end

    for p in stacks
        reverse!(p[2])
    end

    i += 1
    procedure::Vector{NTuple{3,Int}} = []
    for j in i:length(lines)
        words = lines[j] |> split
        cnt, from, to = parse.(Int, (words[2], words[4], words[6]))
        push!(procedure, (cnt, from, to))
    end

    rearrange(deepcopy(stacks), procedure)
    rearrange(deepcopy(stacks), procedure; pack=true)
end

@time day05()
