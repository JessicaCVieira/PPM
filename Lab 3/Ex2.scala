//Ex2

def lazyListRange(lo: Int, hi: Int): LazyList[Int] = {
    println(lo)
    if(lo >= hi) LazyList.empty
    else lo #:: lazyListRange(lo+1, hi)
}
def listRange(lo: Int, hi: Int): List[Int] = {
    println(lo)
    if(lo >= hi) List()
    else lo :: listRange(lo+1, hi)
}