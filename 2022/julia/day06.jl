function detect(line, k)
    for i in k:length(line)
        if length(Set(line[i-k+1:i])) == k
            return i
        end
    end
    return -1
end

function day06()
    line = readline()

    println(detect(line, 4))
    println(detect(line, 14))
end

@time day06()
