object Pattern{ 
    val blinkerRes = (5,5)
    val toadRes = (6,6)
    val gliderRes = (6,6)

    def blinker(): Array[Array[Cell]] = {
        val blinker = Array.ofDim[Cell](blinkerRes._1,blinkerRes._2)
        val pattern = List((2,1),(2,2),(2,3))
        for(r <- 0 until blinkerRes._1;
            c <- 0 until blinkerRes._2){
                if(pattern.contains((r,c))) blinker(r)(c) = Cell(1)
                else blinker(r)(c) = Cell(0)
            }
        blinker
    }

    def toad(): Array[Array[Cell]] = {
        val toad = Array.ofDim[Cell](toadRes._1,toadRes._2)
        val pattern = List((2,2),(2,3),(2,4),(3,1),(3,2),(3,3))
        for(r <- 0 until toadRes._1;
            c <- 0 until toadRes._2){
                if(pattern.contains((r,c))) toad(r)(c) = Cell(1)
                else toad(r)(c) = Cell(0)
            }
        toad
    }

    def glider(): Array[Array[Cell]] = {
        val glider = Array.ofDim[Cell](gliderRes._1,gliderRes._2)
        val pattern = List((1,3),(2,4),(3,2),(3,3),(3,4))
        for(r <- 0 until gliderRes._1;
            c <- 0 until gliderRes._2){
                if(pattern.contains((r,c))) glider(r)(c) = Cell(1)
                else glider(r)(c) = Cell(0)
            }
        glider
    }
}