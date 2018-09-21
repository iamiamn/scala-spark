package circuit_simulation

/**
  * Created by LENOVO on 2016/12/23.
  */
abstract class Gates extends Simulation{
  def InverterDelay: Int
  def AndGateDelay: Int
  def OrGateDelay: Int//先在此声明，可以在另外的trait中定义

  class Wire{
    private var sigVal = false
    private var actions: List[Action] = List()

    def getSignal = sigVal

    def setSignal(s : Boolean) = {
      if (s != sigVal){
        sigVal = s
        actions foreach (_())//!!!相当于for(a <- actions) a()每个action执行，在设置完所有的
      }
    }

    def addAction(a:Action) = {
      actions = a :: actions
      a()//!!!把actions添加到inputWire的List[Action]中，a()是执行action内容，这样是因为a()中有afterDelay,
      //其中有 对outputWire的修改
    }
  }

  def inverter(input: Wire, output: Wire):Unit = {
    def invertAction():Unit={
      val inputSig = input.getSignal
      afterDelay(InverterDelay){
        output setSignal ( !inputSig)//这句到底是afterDelay的第二个参数吗
      }
    }
    input addAction invertAction
  }

  def andGate(in1: Wire, in2: Wire, output: Wire) = {
    def andAction() = {
      val in1Sig = in1.getSignal
      val in2Sig = in2.getSignal//先取signal后再执行afterDelay而不是在
      //afterDelay里面取Signla
      afterDelay(AndGateDelay){
        output setSignal( in1Sig&in2Sig)
      }
    }
    in1 addAction andAction
    in2 addAction andAction
  }

  def orGate(in1:Wire, in2:Wire, output: Wire): Unit = {
    def orAction() = {
      val in1Sig = in1.getSignal
      val in2Sig = in2.getSignal
      afterDelay(OrGateDelay) {
        output setSignal (in1Sig | in2Sig)
      }
    }
    in1 addAction orAction
    in2 addAction orAction
  }

  def orGate2(in1: Wire, in2:Wire, output: Wire): Unit = {
  def orAction(): Unit = {
    afterDelay(OrGateDelay) {
      output setSignal (in1.getSignal | in2.getSignal)
    }
  }
    in1 addAction orAction
    in2 addAction orAction
  }

  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      println(name + " " + currentTime + " new-value = " + wire.getSignal)
    }
    wire addAction probeAction
  }
}
