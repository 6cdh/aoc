function day04()
    lines = readlines()

    # [a, b] includes [c, d]
    include(a, b, c, d) = a <= c && d <= b
    # [a, b] overlaps [c, d]
    overlap(a, b, c, d) = !((b < c) || (d < a))

    cnt = 0
    cnt2 = 0
    for line in lines
        r11, r12, r21, r22 = parse.(Int, split(line, c -> c ∈ ",-"))
        if include(r11, r12, r21, r22) ||
           include(r21, r22, r11, r12)
            cnt += 1
        end
        if overlap(r11, r12, r21, r22)
            cnt2 += 1
        end
    end
    println(cnt)
    println(cnt2)
end

@time day04()
