import os
import re
import subprocess
import csv
import time

titles = ["Protocol", "Unreliable Roles", "Total Comms", "Unreliable Comms", "Refactor Time", "GenX", "Scala Gen Time Before", "Scala Gen Time After"]

def power_set(s):
    if not s:
        return [[]]
    result = power_set(s[1:])
    return result + [subset + [s[0]] for subset in result]


def clean_string(s):
    if '_' in s:
        s = s.split('_')[1]
    s = re.sub(r'\d+$', '', s)
    return s.strip()

def update_signature_roles(input_str):
    words = re.findall(r'\b(?:from|to)\s+(\w+)', input_str)
    unique_words = list(dict.fromkeys(words))  
    role_list = ', '.join(f'role {word}' for word in unique_words)
    result = re.sub(r'\([^)]*\)', f'({role_list})', input_str, count=1)
    return result


def proj_time(filename):
    output = subprocess.run([f"stack exec Teatrino -- --file={filename} --project"], capture_output=True, text=True, shell=True).stdout
    return extract_time(output)


def refactor(input_filename, output_filename, refactor_type):
    refactoring = subprocess.run([f"stack exec Teatrino -- --file={input_filename} --refactor" + refactor_type], capture_output=True, text=True, shell=True).stdout

    if "global" not in refactoring:
        refactoring = "N/A"

    with open(output_filename, "w") as f:
        f.write(update_signature_roles(refactoring))
        
    return refactoring


def time_diff(before, after):
    if before == "N/A" or after == "N/A":
        return "N/A"
    print(before, after)
    return str(int(after) - int(before))


def extract_time(output):
    numbers = [num for num in output.split() if num.strip().isdigit()]
    if numbers:
        return numbers[-1]
    return "N/A"

def average_over_runs(runs):
    averages = []
    for column_values in zip(*runs):
        numeric_values = []
        na_count = 0
        
        for val in column_values:
            if val == "N/A":
                na_count += 1
            else:
                numeric_values.append(int(val))

        if na_count == len(column_values):
            averages.append("N/A")
        else:
            averages.append(sum(numeric_values) / len(numeric_values))
            
    return [round(avg) if isinstance(avg, float) else avg for avg in averages]

def scala_gen_time(filepath):
    output = subprocess.run([f"stack exec Teatrino -- --file={filepath} --effpi"], capture_output=True, text=True, shell=True).stdout
    return extract_time(output)

def scala_gen_time_fo(filepath):
    output = subprocess.run([f"stack exec Teatrino -- --file={filepath} --reffpifo"], capture_output=True, text=True, shell=True).stdout
    return extract_time(output)


def refactor_with_unreliable(filepath, unreliable_roles, refactor_type):
    
    vs = []
    for _ in range(10):
        with open(filepath, 'r') as f:
            protocol = f.read()

        if 'reliable role' in protocol:
            raise Exception("Protocol expected to have no reliable roles")
        
        protocol = protocol.replace(f'role', f'reliable role')

        for role in unreliable_roles:
            protocol = protocol.replace(f'reliable role {role}', f'role {role}')
        
        with open("bench/original.txt", "w") as f:
            f.write(protocol)

        scala_gen_time_before = scala_gen_time("bench/original.txt")
        refactoring = refactor("bench/original.txt", "bench/refactored.txt", refactor_type)
        refactor_time = extract_time(refactoring)

        if refactor_type == "fo":
            scala_gen_time_after = scala_gen_time_fo("bench/original.txt")
        else:
            scala_gen_time_after = scala_gen_time("bench/refactored.txt")

        num_messages_before = protocol.count(' from ')

        num_unreliable_msgs = 0
        for role in unreliable_roles:
            pattern = rf'or\s*\{{\s*crash from {role}'
            num_unreliable_msgs += len(re.findall(pattern, refactoring))

        genx = refactoring.count(' from ') - num_messages_before

        values = [ num_messages_before, num_unreliable_msgs, refactor_time, genx, scala_gen_time_before, scala_gen_time_after ]
        vs.append(values)

    values = ["{ " + ', '.join(unreliable_roles) + " }"] + average_over_runs(vs)

    column_width = max(len(title) for title in titles[1:])

    print("".join(f"{title:<{column_width}} " for title in titles[1:]))
    if values[4] != 0:
        print("".join(f"{value:<{column_width}} " for value in values))
    else:
        print("".join(f"{"N/A":<{column_width}} " for _ in values))

    return values


def run_benchmarks(refactor_type):
    directory = 'bench/protocols'

    todo = []

    for filename in os.listdir(directory):
        file_path = os.path.join(directory, filename)

        with open(file_path, 'r') as f:
            protocol = f.read()
    
        pattern = r"role (\w+)"

        roles = re.findall(pattern, protocol)

        reliability_combinations = [comb for comb in power_set(roles) if comb]

        for combination in reliability_combinations:
            todo.append((file_path, combination))

    results = {}
    for i, (filepath, unreliable_roles) in enumerate(todo):
        filename = filepath.split('/')[2].split('.')[0]
        print(f"running {filepath} with unreliable {unreliable_roles}")
        results[filename + str(i)] = refactor_with_unreliable(filepath, unreliable_roles, refactor_type)


    with open(f"bench/{refactor_type}_output.csv", mode="w", newline="") as file:
        writer = csv.writer(file)

        writer.writerow(titles)
        for key, value in results.items():
            if value[4] != 0:
                writer.writerow([clean_string(key), *value])
            else:
                writer.writerow([clean_string(key)] + ['N/A' for _ in value])


def run_specific_benchmarks(todo, refactor_type, save=True):
    i = 0
    results = {}
    for i, (filepath, unreliable_roles) in enumerate(todo):
        filename = filepath.split('/')[-1].split('.')[0]
        print(f"running {filename} with unreliable {unreliable_roles}")
        results[filename + str(i)] = refactor_with_unreliable(filepath, unreliable_roles, refactor_type)

    if save:
        with open(f"bench/{refactor_type}_output2.csv", mode="w", newline="") as file:
            writer = csv.writer(file)

            writer.writerow(titles)
            for key, value in results.items():
                if value[4] != 0:
                    writer.writerow([clean_string(key), *value])
                else:
                    value[1] = value[2] = value[3] = value[4] = value[5] = value[6] = "N/A"
                    writer.writerow([clean_string(key), *value])

def run_gf_benchmarks():
    run_benchmarks("gf")

def run_lgf_benchmarks():
    run_benchmarks("lgf")

def run_fo_benchmarks():
    run_benchmarks("fo")

def run_nfo_benchmarks():
    run_benchmarks("nfo")

def run_specific_gf_benchmarks(todo, save=True):
    run_specific_benchmarks(todo, "gf", save=save)

def run_specific_lgf_benchmarks(todo, save=True):
    run_specific_benchmarks(todo, "lgf", save=save)

def run_specific_fo_benchmarks(todo, save=True):
    run_specific_benchmarks(todo, "fo", save=save)

def run_specific_nfo_benchmarks(todo, save=True):
    run_specific_benchmarks(todo, "nfo", save=save)

def run_all_benchmarks():
    run_gf_benchmarks()
    run_lgf_benchmarks()
    run_fo_benchmarks()
    run_nfo_benchmarks()


start_time = time.time()

run_specific_gf_benchmarks([
    ("bench/protocols/a_Adder.scr", [ "S" ]),
    ("bench/protocols/a_Adder.scr", [ "S", "Cl" ]),
    
    ("bench/protocols/b_TwoBuyer.scr", [ "P" ]),
    ("bench/protocols/b_TwoBuyer.scr", [ "P", "Q" ]),
    ("bench/protocols/b_TwoBuyer.scr", [ "P", "Q", "R" ]),
   
    ("bench/protocols/c_OAuth.scr", [ "S" ]),
    ("bench/protocols/c_OAuth.scr", [ "S", "A", ]),
    ("bench/protocols/c_OAuth.scr", [ "S", "A", "Cl" ]),

    ("bench/protocols/d_Travel.scr", [ "Cl" ]),
    ("bench/protocols/d_Travel.scr", [ "S" ]),
    ("bench/protocols/d_Travel.scr", [ "Cl", "S" ]),
    ("bench/protocols/d_Travel.scr", [ "Cl", "A", "S" ]),

    ("bench/protocols/e_Logger.scr", [ "Cl" ]),
    ("bench/protocols/e_Logger.scr", [ "I" ]),

    ("bench/protocols/f_CBreaker.scr", [ "A" ]),
    ("bench/protocols/f_CBreaker.scr", [ "S", "R" ]),
    ("bench/protocols/f_CBreaker.scr", [ "S", "A", "R" ]),

    ("bench/protocols/g_DeliveryDriver.scr", [ "Customer" ]),

    ("bench/protocols/h_PaymentProcessor.scr", [ "Customer" ]),

    ("bench/protocols/i_BusNetworkTopology.scr", [ "NodeA", "NodeB", "NodeC", "NodeD", "NodeE", "NodeF", "NodeG", "NodeH", "NodeI", "NodeJ" ]),
    ("bench/protocols/i_BusNetworkTopology.scr", [ "MainCable" ]),

    ("bench/protocols/j_CDN.scr", [ "EdgeServerMEA" ]),
    ("bench/protocols/j_CDN.scr", [ "EUUser1", "NAUser1", "SAUser1", "APACUser1", "MEAUser1" ]),
    
    ("bench/protocols/k_RingNetworkTopology.scr", [ "NodeA" ]),
    ("bench/protocols/k_RingNetworkTopology.scr", [ "NodeA", "NodeB", "Node3", "Node4", "Node5" ]),
    ("bench/protocols/k_RingNetworkTopology.scr", [ "NodeA", "NodeB", "Node3", "Node4", "Node5", "Node6", "Node7", "Node8", "Node9", "Node10" ]),
    
    ("bench/protocols/l_smtp.scr", [ "S", "Cl" ]),
])

run_specific_lgf_benchmarks([
    ("bench/protocols/a_Adder.scr", [ "S" ]),
    ("bench/protocols/a_Adder.scr", [ "S", "Cl" ]),
    
    ("bench/protocols/b_TwoBuyer.scr", [ "P" ]),
    ("bench/protocols/b_TwoBuyer.scr", [ "P", "Q" ]),
    ("bench/protocols/b_TwoBuyer.scr", [ "P", "Q", "R" ]),
   
    ("bench/protocols/c_OAuth.scr", [ "S" ]),
    ("bench/protocols/c_OAuth.scr", [ "S", "A", ]),
    ("bench/protocols/c_OAuth.scr", [ "S", "A", "Cl" ]),

    ("bench/protocols/d_Travel.scr", [ "Cl" ]),
    ("bench/protocols/d_Travel.scr", [ "S" ]),
    ("bench/protocols/d_Travel.scr", [ "Cl", "S" ]),
    ("bench/protocols/d_Travel.scr", [ "Cl", "A", "S" ]),

    ("bench/protocols/e_Logger.scr", [ "Cl" ]),
    ("bench/protocols/e_Logger.scr", [ "I" ]),

    ("bench/protocols/f_CBreaker.scr", [ "A" ]),
    ("bench/protocols/f_CBreaker.scr", [ "S", "R" ]),
    ("bench/protocols/f_CBreaker.scr", [ "S", "A", "R" ]),

    ("bench/protocols/g_DeliveryDriver.scr", [ "Customer" ]),

    ("bench/protocols/h_PaymentProcessor.scr", [ "Customer" ]),

    ("bench/protocols/i_BusNetworkTopology.scr", [ "NodeA", "NodeB", "NodeC", "NodeD", "NodeE", "NodeF", "NodeG", "NodeH", "NodeI", "NodeJ" ]),
    ("bench/protocols/i_BusNetworkTopology.scr", [ "MainCable" ]),

    ("bench/protocols/j_CDN.scr", [ "EdgeServerMEA" ]),
    ("bench/protocols/j_CDN.scr", [ "EUUser1", "NAUser1", "SAUser1", "APACUser1", "MEAUser1" ]),
    
    ("bench/protocols/k_RingNetworkTopology.scr", [ "NodeA" ]),
    ("bench/protocols/k_RingNetworkTopology.scr", [ "NodeA", "NodeB", "Node3", "Node4", "Node5" ]),
    ("bench/protocols/k_RingNetworkTopology.scr", [ "NodeA", "NodeB", "Node3", "Node4", "Node5", "Node6", "Node7", "Node8", "Node9", "Node10" ]),
    
    ("bench/protocols/l_smtp.scr", [ "S", "Cl" ]),
])

run_specific_fo_benchmarks([
    ("bench/protocols/a_Adder.scr", [ "S" ]),
    ("bench/protocols/a_Adder.scr", [ "S", "Cl" ]),
    
    ("bench/protocols/b_TwoBuyer.scr", [ "P" ]),
    ("bench/protocols/b_TwoBuyer.scr", [ "P", "Q" ]),
    ("bench/protocols/b_TwoBuyer.scr", [ "P", "Q", "R" ]),
   
    ("bench/protocols/c_OAuth.scr", [ "S" ]),
    ("bench/protocols/c_OAuth.scr", [ "S", "A", ]),
    ("bench/protocols/c_OAuth.scr", [ "S", "A", "Cl" ]),

    ("bench/protocols/d_Travel.scr", [ "Cl" ]),
    ("bench/protocols/d_Travel.scr", [ "S" ]),
    ("bench/protocols/d_Travel.scr", [ "Cl", "S" ]),
    ("bench/protocols/d_Travel.scr", [ "Cl", "A", "S" ]),

    ("bench/protocols/e_Logger.scr", [ "Cl" ]),
    ("bench/protocols/e_Logger.scr", [ "I" ]),

    ("bench/protocols/f_CBreaker.scr", [ "A" ]),
    ("bench/protocols/f_CBreaker.scr", [ "S", "R" ]),
    ("bench/protocols/f_CBreaker.scr", [ "S", "A", "R" ]),

    ("bench/protocols/g_DeliveryDriver.scr", [ "Customer" ]),

    ("bench/protocols/h_PaymentProcessor.scr", [ "Customer" ]),

    ("bench/protocols/i_BusNetworkTopology.scr", [ "NodeA", "NodeB", "NodeC", "NodeD", "NodeE", "NodeF", "NodeG", "NodeH", "NodeI", "NodeJ" ]),
    ("bench/protocols/i_BusNetworkTopology.scr", [ "MainCable" ]),

    ("bench/protocols/j_CDN.scr", [ "EdgeServerMEA" ]),
    ("bench/protocols/j_CDN.scr", [ "EUUser1", "NAUser1", "SAUser1", "APACUser1", "MEAUser1" ]),
    
    ("bench/protocols/k_RingNetworkTopology.scr", [ "NodeA" ]),
    ("bench/protocols/k_RingNetworkTopology.scr", [ "NodeA", "NodeB", "Node3", "Node4", "Node5" ]),
    ("bench/protocols/k_RingNetworkTopology.scr", [ "NodeA", "NodeB", "Node3", "Node4", "Node5", "Node6", "Node7", "Node8", "Node9", "Node10" ]),
    
    ("bench/protocols/l_smtp.scr", [ "S", "Cl" ]),
])


end_time = time.time()
elapsed_time = end_time - start_time
print(f"\nTotal execution time: {elapsed_time:.2f} seconds")


