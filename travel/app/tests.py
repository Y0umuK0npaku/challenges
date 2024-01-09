import subprocess
import random
import unittest

def generate_test_case():
    n = random.randint(1, 50)
    x = random.randint(2, 100)
    a = sorted(random.sample(range(1, x), n))
    return n, x, a

def solve_case(n, x, a):
    distances = [a[i+1] - a[i] for i in range(n-1)] + [x - a[-1], x - a[-1]]
    last_distance = (x - a[-1]) * 2
    max_distance = max(distances)
    return max(max_distance, last_distance)


def run_haskell_script(input_data):
    command = ["runhaskell", "Main.hs"]
    process = subprocess.Popen(command, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                               text=True)

    output, _ = process.communicate(input_data)
    if error:
        print("Error running Haskell script:", error)
    else:
        return output.strip()

class TestHaskellProgram(unittest.TestCase):
    
    def test_random_cases(self):
        for _ in range(10):  # number of random test cases
            n, x, a = generate_test_case()
            input_data = f"{1}\n{n} {x}\n{' '.join(map(str, a))}\n"
            print(run_haskell_program(input_data).strip())

if __name__ == '__main__':
    unittest.main()
