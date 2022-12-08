# ------------------------------------------------------------------------
# 1 - A 
# ------------------------------------------------------------------------
def dfs(graph, start, visited, ordering):
    visited.add(start)
    for neighbor in graph[start]:
        if neighbor not in visited:
            dfs(graph, neighbor, visited, ordering)
    ordering.append(start)

def topological_sort(graph):
    visited = set()
    ordering = []
    for node in graph:
        if node not in visited:
            dfs(graph, node, visited, ordering)
    return reversed(ordering)

def printOrder(ORDER):
    for item in ORDER:
        print(item)

# ------------------------------------------------------------------------




# ------------------------------------------------------------------------
# 1 - B
# ------------------------------------------------------------------------
def topological_ordering(graph):
  order = []  # list to store the topological ordering
  visited = set()  # set to keep track of visited nodes
  
  # Function that recursively traverses the graph starting from a given node
  def traverse(node):
    # If the node has not been visited yet, add it to the order and mark it as visited
    if node not in visited:
      order.append(node)
      visited.add(node)
      
      # Consider all nodes that can be reached from the current node
      for neighbor in graph[node]:
        traverse(neighbor)
  
  # Start the traversal from all nodes in the graph, one by one
  for node in graph:
    traverse(node)
  
  return order


# ------------------------------------------------------------------------
# 2
# ------------------------------------------------------------------------
def function_pow(a, n):
    result = 1
    while n > 0:
        if n % 2 == 1:
            result = result * a
        a = a * a
        n = n / 2
    return result

# ------------------------------------------------------------------------
# 3
# ------------------------------------------------------------------------


def solve_sudoku(grid):
    def is_valid_choice(grid, row, col, choice):
        # check if the given choice is valid for the given row and column
        for i in range(9):
            # check if the choice is already used in the given row or column
            if grid[row][i] == choice or grid[i][col] == choice:
                return False

        # check if the choice is already used in the 3x3 subgrid
        for i in range(3):
            for j in range(3):
                if grid[(row // 3) * 3 + i][(col // 3) * 3 + j] == choice:
                    return False

        # if none of the above checks failed, the choice is valid
        return True

    def solve(grid):
        for row in range(9):
            for col in range(9):
                # check if the current cell is empty
                if grid[row][col] == 0:
                    # try every possible choice for the empty cell
                    for choice in range(1, 10):
                        if is_valid_choice(grid, row, col, choice):
                            # if the choice is valid, update the grid with the new value
                            grid[row][col] = choice
                            # recursively try to solve the updated grid
                            if solve(grid):
                                return True
                            # if the updated grid cannot be solved, revert the change and try the next choice
                            grid[row][col] = 0
                    # if no choice worked, return False to backtrack
                    return False
        # if the grid is full, it means we have successfully solved the puzzle
        return True

    # try to solve the grid using the above recursive function
    if solve(grid):
        return grid
    else:
        return None














def DRIVER():

    # ---------------------------------------------
    # ---> 1 : A - B
    # ---------------------------------------------
    
    graph = {
        102: [241],

        241: [222],

        222: [321],

        211: [321],

        321: [422],

        422: []
    } 
    print("\n")

    print("-----------------------------------")
    print("---> 1 - A")
    print("-----------------------------------")
    #  ---> 1 - A
    ORDER = topological_sort(graph)
    printOrder(ORDER)
    print("\n")

    print("-----------------------------------")
    print("---> 1 - B")
    print("-----------------------------------")
    #  ---> 1 - B
    print(topological_ordering(graph))



    # ---------------------------------------------
    # ---> 2
    # ---------------------------------------------
    print("\n")
    print("-----------------------------------")
    print("---> 2")
    print("-----------------------------------")

    a_number = int(input("please enter a number:"))

    n_number = int(input("please enter n number:"))

    print(function_pow(a_number,n_number))



    # ---------------------------------------------
    # ---> 3
    # ---------------------------------------------
    print("\n")
    print("-----------------------------------")
    print("---> 3")
    print("-----------------------------------")

    grid = [[5,1,7,6,0,0,0,3,4], #0

        [2,8,9,0,0,4,0,0,0], #1

        [3,4,6,2,0,5,0,9,0], #2

        [6,0,2,0,0,0,0,1,0], #3

        [0,3,8,0,0,6,0,4,7], #4

        [0,0,0,0,0,0,0,0,0], #5

        [0,9,0,0,0,0,0,7,8], #6

        [7,0,3,4,0,0,5,6,0], #7

        [0,0,0,0,0,0,0,0,0]] #8

    solved_grid = solve_sudoku(grid)
    print(solved_grid)


# RUN DRIVER DEF FUNCTION
DRIVER()
