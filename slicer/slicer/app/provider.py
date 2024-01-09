import subprocess
import random

def run_haskell_script(n, m):
    # Prepare the command to run the Haskell script
    command = ["runhaskell", "Main.hs"]

    # Start the Haskell script process
    process = subprocess.Popen(command, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)

    # Send 'n' and 'm' to the Haskell script and get the output
    output, error = process.communicate(input=f"{n}\n{m}\n")
    
    if error:
        print("Error running Haskell script:", error)
    else:
        return output.strip()

def test_haskell_script(num_tests=10):
    for _ in range(num_tests):
        # Generate random values for n and m where n < m
        n = random.randint(-100, 100)
        m = random.randint(n + 1, 2000000)

        # Run the Haskell script with these values
        result = run_haskell_script(n, m)

        print(f"Test with n = {n}, m = {m}, Result: {result}")

if __name__ == "__main__":
    test_haskell_script()
