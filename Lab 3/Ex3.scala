//Ex 3 
// a)
def ex3(a: List[Int], b: List[Int]) : List[Int] = 
{
    if(a.length != b.length) Nil
    def loop(a1: List[Int], b1: List[Int], n: Int) : List[Int] = 
    { 
        a1 match{
            case Nil => Nil
            case x :: xs => x + b1.head :: loop(xs, b1.tail, n-1)
        }
    }
    loop(a, b, a.length)
} 

//mais simples (ñ era preciso o loop)
def ex3a(a: List[Int], b: List[Int]) : List[Int] = 
{
    if(a.length != b.length) Nil
    else 
        a match{
            case Nil => Nil
            case x :: xs => x + b.head :: ex3a(xs, b.tail)
        } 
} 

//b) with loop 
def zipWith[A, B, C](op: (A, B) => C, a: List[A], b: List[B]) : List[C] = 
{
    if(a.length != b.length) Nil
    def loop(a1: List[A], b1: List[B], n: Int) : List[C] = 
    { 
        a1 match{
            case Nil => Nil
            case x :: xs => op(x, b1.head) :: loop(xs, b1.tail, n-1)
        }
    }
    loop(a, b, a.length)
}

//b) without loop
def zipWith2[A, B, C](op: (A, B) => C, a: List[A], b: List[B]) : List[C] = 
{
    if(a.length != b.length) Nil
    else
        a match{
            case Nil => Nil
            case x :: xs => op(x, b.head) :: zipWith2(op, xs, b.tail)
        }
}

//c)
def isSorted[A](lst: List[A], ordered: (A,A) => Boolean): Boolean = 
{
    lst match {
        case Nil => true // the teacher does not especify what to do in this case
        case List(_) => true // the teacher does not especify what to do in this case
        case head :: second :: tail => ordered(head,second) && isSorted(tail, ordered)
    }
}

//d)
def bubbleSort(data: List[Int], f: (Int, Int) => Boolean): List[Int] = 
{
    def sort(lst: List[Int]) : List[Int] = 
    {
        lst match 
        {
            case Nil => Nil
            case List(_) =>  lst
            case head :: second :: tail => 
                if (f(head, second)) head :: sort(second :: tail) 
                else second :: sort(head :: tail)
        }
    }

    def loop (lst: List[Int], n: Int) : List[Int] = 
    {
        if (n <= 0) lst
        else loop(sort(lst), n - 1)        
    }   

    loop(data, data.length)
}