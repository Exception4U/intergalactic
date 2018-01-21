/**This code is designed to solve problem described as "Merchant's Guide To The Galaxy"
  *in thought works problem sheet.
  *
  * Author: Tushar Vaidya
  * Author Contact: mail@tusharvaidya.info , tushar5610@gmail.com
  *
  *Code API : This code require input file as STDIN and it will generate output in STDOUT.
  *For example following linux command will take input.txt file where input is stored and will 
  *generate output.out file ----->
  *                          
  *           scala intergalactic.scala < input.txt &> output.out 
  *
  *Code Assumptions :As in problem statement only description and input cases with output
  *cases are given I made best possible way to judge logic by mapping input with output.It can 
  *hadle invalid inputs as well as invalid intergalactic numbers as describe in std input file.
  *
  *Disclaimer : This code does not 100 % follow ethics of functional programming , we may make it 
  *more colse enough to it :) However it solves the given problem satisfiably.
  *
  */
import io.Source._
import scala.collection.mutable._

object InterGalactic {

    val digits = {
        scala.collection.immutable.Map("i"-> 1,"v"-> 5,"x"-> 10,"l"-> 50,"c"-> 100,"d"-> 500,"m"-> 1000)
    }
    var num = new scala.collection.mutable.HashMap[String,Int]
    var goods = new scala.collection.mutable.HashMap[String,Float]

    def msg(s:String){
          println(s)
      }

    def printBuffer(word:ArrayBuffer[String]):String = {
        /*prints Array Buffer of string type*/
        var p:String = ""
        for( i <- word){ p += i; p+= " " }
        return p
    }    

    def parse(word:ArrayBuffer[String]):Int = {
        /*Parse a Galactic/Roman number*/
        var n:Int = 0
        var dig = new ArrayBuffer[Int]
        try { 
            dig = (for (i <- word) yield num(i))
        } 
        catch {
            case e: Exception => return -1
        }
        while (dig.nonEmpty) {
            var d = dig(0)
            dig.trimStart(1)
            if (dig.nonEmpty && dig(0) > d){
                n -= d
            }
            else{
                n += d
            }
        }
        
    return n
    }

             

    def process(line:String) {
        /*Read a sentence and categorise teh sentance to process further*/
        
        var word1 = ((line.toLowerCase).split(" ")).toBuffer
        var word = new ArrayBuffer[String]
        var i:Int =0 ; while(i < word1.length){word += word1(i); i+=1;};
        if (!word.nonEmpty)
            return
            
        if (word.length == 3 && word(1) == "is") {
            val key = word(0)
            var valu = word(2)
            
            if (!digits.contains(valu)){
                msg("'%s' is not a valid digit." format valu)
                return
            }
            num(key) = digits(valu)
            return
        }
        
        var n:Int = 0    
        
        if (word.length > 4 && word.last == "credits" && word(word.length - 3) == "is") {
            word.trimEnd(1)
            var valu:Float = 0
            try{
               valu = (word.last).toFloat
            }
            catch{
             case e:Exception =>
             msg("'%s' is not a valid numeric value" format word.last)
             return
                }
                
            word.trimEnd(1)
            word.trimEnd(1)
            var g = (word.last)
            word.trimEnd(1)
            n = parse(word)
            
            if (n < 0){
                msg("That's not a valid Galactic number")
                return
            }
            goods(g) = (valu / n)
            return 
        }
           
        if (word.containsSlice(ArrayBuffer("how", "much", "is"))) {
            word = word.slice(3,word.length)
            if (word.last == "?"){
                word.trimEnd(1)
            }
                
            n = parse(word)
            if (n < 0){
                msg(printBuffer(word) + "is not a valid Galactic number")
                return
            }
                
            msg( printBuffer(word) + " is "+ n.toString)
            return  
        }
            
        if (word.containsSlice(ArrayBuffer("how", "many", "credits", "is"))) {
            word = word.slice(4,word.length)
            if (word.last == "?"){
                word.trimEnd(1)
            }
                
            var g = (word.last)
            word.trimEnd(1)
            if (!goods.contains(g)){
                msg("I don't know of the trading good %s" format g)
                return
            }
                
            n = parse(word)
            if (n < 0){
                msg("'%s' is not a valid Galactic number" format word)
                return
            }
            msg((printBuffer(word) )+ (g) +" " + "is " + (n * goods(g)).toString + " Credits")
            return
        }
            
        msg("I've no idea what you are talking about")
        return
    }

    def main(args: Array[String]) {
        for(line <- stdin.getLines){
            process(line)
          } 
    }

}
