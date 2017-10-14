package latmap

import org.scalatest.FunSuite

class LongSolverTest extends FunSuite {
  test("SUopt") {
    val p = API()

    {
      import p._

      // Inputs
      val AddrOf = relation(2)
      val Copy = relation(2)
      val Store = relation(3)
      val Load = relation(3)
      val CFG = relation(2)
      val Multi = relation(1)
      val Phi = relation(1)
      val Clear = relation(1)
      val FIStore = relation(3)
      val FILoad = relation(3)

      // Outputs
      val Pt = relation(2)

      val SU = relation(2, SULattice)

      val PtH = relation(2)
      val Kill = relation(1, SULattice)

      // Rules
      // ----------
      //
      val pvar = variable()
      val a = variable()
      val b = variable()
      val k = variable()
      val l = variable()
      val q = variable()
      val t = latVariable(SULattice)
      val z = latVariable(SULattice)

      // AddrO
      Pt(p,a) :- AddrOf(p,a)

      // Copy
      Pt(p,a) :- (Copy(p,q), Pt(q,a))

      // Store
      def toSingle(x: String) = SULattice.Single(x)
      SU(l,a,z) :- (Store(l,pvar,q), Pt(pvar,a), Pt(q,b), T(z, toSingle, b))

      PtH(a,b) :- (Store(l,pvar,q), Pt(p,a), Pt(q,b))
      PtH(a,b) :- (FIStore(p,q,l), Pt(p,a), Pt(q,b))

      // Load
      def filter(e: SULattice.Elem, p:String): Boolean = e match {
        case SULattice.Bottom => false
        case SULattice.Single(s) => p == s
        case SULattice.Top => true
      }

      Pt(p,b) :- (Load(l,p,q), Pt(q,a), F(filter,t,b), PtH(a,b), SU(l,a,t))
      Pt(p,b) :- (FILoad(p,q,l), Pt(q,a), PtH(a,b))

      // Preserve
      def killNot(a: String, e: SULattice.Elem): Boolean = e match {
        case SULattice.Bottom => false
        case SULattice.Single(s) => a != s
        case SULattice.Top => true
      }
      SU(q,a,t) :- (CFG(l, q), SU(l,a,t), Multi(a))
      SU(q,a,t) :- (CFG(l, q), SU(l,a,t), F(killNot,a,z), Kill(q, z))

      // PtSU
      SU(l,a,z) :- (Clear(l), PtH(a,b), T(z,toSingle,b))

      // Kill
      Kill(l,z) :- (Store(l,pvar,q), Pt(p,b), T(z,toSingle,b))
      Kill(l, SULattice.Top) :- Phi(l)

      // Example facts
      AddrOf("p","a") :- ()
      AddrOf("mb","b") :- ()
      Store("l1","p","mb") :- ()
      Load("l1","q","p") :- ()
      AddrOf("mc","c") :- ()
      Store("l3","p","mc") :- ()
      Load("l3","r","p") :- ()
      AddrOf("p2","d") :- ()
      AddrOf("mf","f") :- ()
      Store("l5","p2","mf") :- ()
      Phi("l6") :- ()
      Copy("p3","p") :- ()
      Copy("p3","p2") :- ()
      Load("l6","s","p3") :- ()
      AddrOf("me","e") :- ()
      Store("l7","p3","me") :- ()
      Load("l7","t","p3") :- ()
      CFG("l1","l3") :- ()
      CFG("l3","l5") :- ()
      CFG("l3","l6") :- ()
      CFG("l5","l6") :- ()
      CFG("l6","l7") :- ()
    }

    println(p.asInstanceOf[APIImpl].program)

    p.solve()
  }

}
