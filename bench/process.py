import sys

if len(sys.argv) < 2:
    print("usage: " + sys.argv[0] + " results")
    exit(1)

def parse_time(line : str):
    tokens = line.split()
    real_time = tokens[5]
    return int(real_time)

def parse_exit(line : str):
    tokens = line.split()
    return tokens[2]

filename = sys.argv[1]

with open(filename) as f:
    f.readline() # skip first empty line

    results = []
    total_times = {
        "plain" : {},
        "skolem" : {},
        "symmetry" : {},
        "sat" : {},
        "skolem+symmetry" : {},
        "skolem+sat" : {},
        "symmetry+sat" : {},
        "skolem+symmetry+sat" : {}
    }
    solve_times = {
        "plain" : {},
        "skolem" : {},
        "symmetry" : {},
        "sat" : {},
        "skolem+symmetry" : {},
        "skolem+sat" : {},
        "symmetry+sat" : {},
        "skolem+symmetry+sat" : {}
    }
    completes = {
        "plain" : {},
        "skolem" : {},
        "symmetry" : {},
        "sat" : {},
        "skolem+symmetry" : {},
        "skolem+sat" : {},
        "symmetry+sat" : {},
        "skolem+symmetry+sat" : {}
    }
    bench_names = []
    for bench in f:
        skolem = "skolem" in bench
        sym = "symmetry" in bench
        sat = "sat" in bench
        bench_name = (bench.split(".")[0]).split("-")[0]
        completed = 1

        if bench_name not in bench_names:
            bench_names.append(bench_name)

        total_time = 0
        solve_time = 0

        if skolem:
            f.readline() # skolemizing...
            total_time = total_time + parse_time(f.readline())

        f.readline() # instantiating bounds...
        total_time = total_time + parse_time(f.readline())

        if sym:
            f.readline() # breaking symmetry...
            total_time = total_time + parse_time(f.readline())
        
        f.readline() # making boolean interpretation...
        total_time = total_time + parse_time(f.readline())

        if sat:
            exit_code = "10"

            f.readline() # making optimized SAT call...
            maybe_time = f.readline() # starting solver... OR cpu ...
            if maybe_time.split()[0] == "cpu":
                solve_time = parse_time(maybe_time)
            else:
                f.readline() # CNF-SAT instance...
                f.readline() #   originally had...
                exit_code = parse_exit(f.readline())
                solve_time = parse_time(f.readline())

            if exit_code != "10" and exit_code != "20":
                completed = 0
            #else:
            total_time = total_time + solve_time
        else:
            f.readline() # making Rosette solver call..
            solve_time = parse_time(f.readline())
            total_time = total_time + solve_time

        f.readline() # newline

        key = ''
        if skolem and sym and sat:
            key = 'skolem+symmetry+sat'
        elif (not skolem) and sym and sat:
            key = 'symmetry+sat'
        elif skolem and (not sym) and sat:
            key = 'skolem+sat'
        elif skolem and sym and (not sat):
            key = 'skolem+symmetry'
        elif (not skolem) and (not sym) and sat:
            key = 'sat'
        elif (not skolem) and sym and (not sat):
            key = 'symmetry'
        elif skolem and (not sym) and (not sat):
            key = 'skolem'
        else:
            key = 'plain'

        total_times[key][bench_name] = total_time
        solve_times[key][bench_name] = solve_time
        completes[key][bench_name] = completed
    
    header = 'opts'
    for name in bench_names:
        header = header + ',' + name
    
    fn_prefix = sys.argv[1].split('.')[0]
    with open(fn_prefix + '-solve-times.csv', 'w') as stf:
        stf.write(header + '\n')
        for opt, times in solve_times.items():
            row = opt
            for name in bench_names:
                row = row + ',' + str(times[name])
            stf.write(row + '\n')
    with open(fn_prefix + '-total-times.csv', 'w') as ttf:
        ttf.write(header + '\n')
        for opt, times in total_times.items():
            row = opt
            for name in bench_names:
                row = row + ',' + str(times[name])
            ttf.write(row + '\n')
    with open(fn_prefix + '-completes.csv', 'w') as cf:
        cf.write(header + '\n')
        for opt, times in completes.items():
            row = opt
            for name in bench_names:
                row = row + ',' + str(times[name])
            cf.write(row + '\n')
