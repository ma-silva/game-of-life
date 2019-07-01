import scala.util.Random
import Pattern._

object Main{
    val gridRows = 6
    val gridCols = 6

    def main(args: Array[String]): Unit = {
        println("running..")

        // var present = Array.ofDim[Cell](gridRows,gridCols)
        // for(r <- 0 until gridRows;
        //     c <- 0 until gridCols){
        //         present(r)(c) = Cell(Random.nextInt(2))
        //     }
        var present = glider()

        printGeneration(present)

        while(true){
            var future = Array.ofDim[Cell](gridRows,gridCols)
            for(r <- 0 until gridRows;
                c <- 0 until gridCols){
                    val neighbors = countNeighbors(present,r,c)
                    val state = present(r)(c)

                    if(state.state == 0 && neighbors==3)
                        future(r)(c) = Cell(1)
                    else if(state.state == 1 && (neighbors<2 || neighbors>3))
                        future(r)(c) = Cell(0)
                    else
                        future(r)(c) = state
                }

            Thread.sleep(1500)

            println()
            printGeneration(future)
            present = future
        }
    }
    
    def countNeighbors(grid: Array[Array[Cell]], row: Int, col: Int): Int = {
        val count = for(r <- -1 until 2;
            c <- -1 until 2) yield {
                val gr = (r + row + gridRows) % gridRows
                val gc = (c + col + gridCols) % gridCols
                if(grid(gr)(gc).state == 1) 1 else 0
            }
        count.sum - grid(row)(col).state
    }

    def printGeneration(grid: Array[Array[Cell]]): Unit = {
        for(r <- 0 until gridRows;
            c <- 0 until gridCols){
                if(c==gridRows-1) println(grid(r)(c).toString)
                else print(grid(r)(c).toString)
            } 
    }
}

case class Cell(state: Int){
    override def toString(): String = if(state == 1) "[+]"
    else "[ ]"
}