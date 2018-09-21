import myfrp._

var a = Var(1)
var b = Signal(a() + 1)
a()
b()
b.getCaller//caller的values在计算b()值时，把b写入了values队头，计算出b的值后就删除caller的对头
//caller是共有的，放在object Signla中
a.getCaller
a.getObs
a() = 2
a()
b()

