import circuit_simulation.{Circuits, Parameters}
//package circuit_simulation

//object test {
//  println("welcome to the scala workSheet")
//  object sim extends Circuits with Parameters
//  import sim._//这句系统会自动帮你清掉？？
//  val in1, in2, sum, carry = new Wire
//
//  halfAdder(in1, in2, sum, carry)
//  probe("sum", sum)
//  probe("carry", carry)
//  in1 setSignal true
//  run()//????
//
//
//  in1 setSignal true
//  run()
//
//  in2 setSignal true
//  run()
//}
//pacakage 改成import, 然后把所有语句都从object test的封装中拿出来
println("welcome to the scala workSheet")
object sim extends Circuits with Parameters
import sim._//这句系统会自动帮你清掉？？
val in1, in2, sum, carry = new Wire

halfAdder(in1, in2, sum, carry)
probe("sum", sum)
probe("carry", carry)
in1 setSignal true
//agenda.length//???为什么无法访问agenda
carry.getSignal
sum.getSignal
in1.getSignal
in2.getSignal//SigVal默认是false,所以in2的值现在为false



run()//run函数属于simulation母类
currentTime

//最高类，Gate类继承simulation，orGate,andGate等，对于output wire的操作都记载在
//simulation类的agenda中，run则执行agenda。
//例如设置in1 setSignal true, run执行agenda时便会执行orGate等方法加入到agenda中的getSignal函数
carry.getSignal
sum.getSignal
in1.getSignal
in2.getSignal//SigVal默认是false,所以in2的值现在为false

//对比后可以知道run()是执行对outputWire的修改，因为afterDelay的操作都会加入到agenda中

in1 setSignal true
run()

in2 setSignal true
run()