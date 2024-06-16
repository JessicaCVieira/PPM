//Ex: 3

type Pol = List[(Float, Int)] 

//ordem crescente 1,2,3,4 do x
def order(p: Pol) : Pol = 
{
  p match{
    case Nil => Nil
    case head :: Nil => List(head)
    case head :: second :: next => {
      if(head._2 < second._2){
        head :: order(second::next)
      }else{
        second :: order(head::next)
      }
    }
  }
}

def sameDegree(e:(Float, Int), p: Pol) : Boolean = 
{
  (p foldRight false) ((head, p) => 
    if(head._2 == e._2)
      true
    else 
      p
  )
}

def sumDeg(e:(Float, Int), p: Pol) : Pol = 
{
  (p foldRight List[(Float, Int)]()) ((head, p) => 
    if(head._2 == e._2)
      ((head._1 + e._1), head._2) :: p
    else 
      head :: p
  )
}

def sum(a: Pol, b: Pol) : Pol = 
{
  (a foldRight b) ((head, a) => 
    if(!sameDegree(head, b))
      head::a
    else 
      sumDeg(head, a)
  )
}