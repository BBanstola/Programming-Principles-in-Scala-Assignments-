/*  The numbers at the edge of the triangle are all 1, and each number inside the triangle is
    the sum of the two numbers above it. Do this exercise by implementing the pascal function
    , which takes a column c and a row r, counting from 0 and returns the number at that
    spot in the triangle. */


/*        1
         121
        1331
       14641   is an example of a Pascal's Triangle      */


def pascal(row: Int, col: Int):Int ={
  if (row < 1 && col < 1 ) throw new Error ("Entry must be positive")
  else if (col > row + 1) throw new Error ("No such value exist")
  else if (col == 1 || col == row + 1) 1
  else pascal(row - 1, col -1) + pascal(row - 1, col)
}

println(pascal (6,4))                  //gives 20



