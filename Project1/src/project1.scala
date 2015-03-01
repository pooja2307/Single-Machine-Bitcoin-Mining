import akka.actor._
import akka.routing.RoundRobinRouter
import java.security.MessageDigest

object project1 extends App
{
  generate(numofworkers = 20)
  
  case class RandomStr(startingzero: Int)
  case class HashGen(str: String, startingzero: Int)
  
  class Worker extends Actor
  {
    val z = "0000000000000000000000000000000000000000000000000000000000000000"
   
    def BitcoinGen(str: String):String = 
    {
       	val md = MessageDigest.getInstance("SHA-256")
       	md.update(str.getBytes())
       	val hash = md.digest()
       	val sb = new StringBuffer()
       	var i = 0
       	while (i < hash.length)
       	{
       		sb.append(Integer.toString((hash(i) & 0xff) + 0x100, 16).substring(1))
       		i = i+1
       	}
       	return sb.toString()
    }
    
    def receive = {
      case HashGen(str, k) =>
      	var finalhash = BitcoinGen(str)
      	if (finalhash.substring(0,k) == z.substring(0,k))
      	{
      		println(str+"\t\t"+finalhash)
      		context.system.shutdown()
      	}
    }    
  }
  
  class Master(numofworkers: Int) extends Actor
  {
    val WorkerRouter = context.actorOf(Props[Worker].withRouter(RoundRobinRouter(numofworkers)), name = "WorkerRouter")
    
    def randomStringGen(L: Int):String =
    {
      val ran = new scala.util.Random
      val sb1 = new StringBuilder
      for (i <- 1 to L) 
      {
      	sb1.append(ran.nextPrintableChar)
      }
      return sb1.toString()  
    }
    
    def receive = {
      
      case RandomStr(k) =>
        var str1 = randomStringGen(32)
        var str = "apraharaj24".concat(str1)
        WorkerRouter ! HashGen(str, k)
    }
  }
  
  def generate(numofworkers: Int)
  {
    val system = ActorSystem("BitcoinGenerator")
    
    val master = system.actorOf(Props(new Master(numofworkers)), name = "master")
    
    println("Enter number of starting zeroes: ")
    val k = Console.readInt()
    while(true)
    {
      master ! RandomStr(k)
    }
  }
}