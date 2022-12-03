function priority(c)
    if islowercase(c)
        c - 'a' + 1
    else
        c - 'A' + 27
    end
end

function day03()
    lines = readlines()

    s = 0
    for line in lines
        n = length(line)
        half1 = line[1:n÷2]
        half2 = line[n÷2+1:n]
        items = half1 ∩ half2

        s += priority(items[1])
    end
    println(s)

    s2 = 0
    for i in 1:3:length(lines)
        a, b, c = lines[i], lines[i+1], lines[i+2]
        items = ∩(a, b, c)

        s2 += priority(items[1])
    end
    println(s2)
end

@time day03()
