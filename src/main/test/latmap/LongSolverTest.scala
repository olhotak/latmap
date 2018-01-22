package latmap

import org.scalatest.FunSuite

class LongSolverTest extends FunSuite {
  class SUProgram {
    val api = API()
    import api._

    // Inputs
    val AddrOf = relation(2, "AddrOf")
    val Copy = relation(2, "Copy")
    val Store = relation(3, "Store")
    val Load = relation(3, "Load")
    val CFG = relation(2, "CFG")
    val Multi = relation(1, "Multi")
    val Phi = relation(1, "Phi")
    val Clear = relation(1, "Clear")
    val FIStore = relation(3, "FIStore")
    val FILoad = relation(3, "FILoad")

    // Outputs
    val Pt = relation(2, "Pt")

    val pvar = variable()
    val a = variable()
    val b = variable()
    val k = variable()
    val l = variable()
    val q = variable()
    val t = latVariable(SULattice)
    val z = latVariable(SULattice)

    // Indexes
//    AddrOf(a,b).addIndex(a,b)
//    Copy(a,b).addIndex(a,b)
    Copy(a,b).addIndex(a)
//    Store(a,b,k).addIndex(a,b,k)
    Store(a,b,k).addIndex(b)
    Store(a,b,k).addIndex(k)
//    Load(a,b,k).addIndex(a,b,k)
    Load(a,b,k).addIndex(a)
    Load(a,b,k).addIndex(k)
//    CFG(a,b).addIndex(a,b)
    CFG(a,b).addIndex(a)
    CFG(a,b).addIndex(b)
    FIStore(a,b,k).addIndex(a)
    FIStore(a,b,k).addIndex(b)
    FILoad(a,b,k).addIndex(b)

//    Pt(a,b).addIndex(a,b)
    Pt(a,b).addIndex(a)

    val SU = relation(2, SULattice, "SU")
//    SU(a,b,t).addIndex(a,b)
    SU(a,b,t).addIndex(a)

    val PtH = relation(2, "PtH")
//    PtH(a,b).addIndex(a,b)
    PtH(a,b).addIndex(a)
    val Kill = relation(1, SULattice, "Kill")

    // Rules
    // ----------
    //


    // AddrO
    Pt(pvar,a) :- AddrOf(pvar,a)

    // Copy
    Pt(pvar,a) :- (Copy(pvar,q), Pt(q,a))

    // Store
    def toSingle(x: String) = SULattice.Single(x)
    SU(l,a,z) :- (Store(l,pvar,q), Pt(pvar,a), Pt(q,b), T(z, toSingle, b))

    PtH(a,b) :- (Store(l,pvar,q), Pt(pvar,a), Pt(q,b))
    PtH(a,b) :- (FIStore(pvar,q,l), Pt(pvar,a), Pt(q,b))

    // Load
    def filter(e: SULattice.Elem, p:String): Boolean = e match {
      case SULattice.Bottom => false
      case SULattice.Single(s) => p == s
      case SULattice.Top => true
    }

    Pt(pvar,b) :- (Load(l,pvar,q), Pt(q,a), F(filter,t,b), PtH(a,b), SU(l,a,t))
    Pt(pvar,b) :- (FILoad(pvar,q,l), Pt(q,a), PtH(a,b))

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
    Kill(l,z) :- (Store(l,pvar,q), Pt(pvar,b), T(z,toSingle,b))
    Kill(l, SULattice.Top) :- Phi(l)

    val predicateNames = Map(
      ("AddrOf", AddrOf),
      ("Copy", Copy),
      ("Load", Load),
      ("Store", Store),
      ("CFG", CFG),
      ("Multi", Multi),
      ("Phi", Phi),
      ("Clear", Clear),
      ("FILoad", FILoad),
      ("FIStore", FIStore),
      ("Pt", Pt),
      ("SU", SU),
      ("PtH", PtH),
      ("Kill", Kill)
    )
  }

  test("StrongUpdate example") {
    val program = new SUProgram

    {
      import program._, program.api._

      // Example facts
      AddrOf("p", "a") :- ()
      AddrOf("mb", "b") :- ()
      Store("l1", "p", "mb") :- ()
      Load("l1", "q", "p") :- ()
      AddrOf("mc", "c") :- ()
      Store("l3", "p", "mc") :- ()
      Load("l3", "r", "p") :- ()
      AddrOf("p2", "d") :- ()
      AddrOf("mf", "f") :- ()
      Store("l5", "p2", "mf") :- ()
      Phi("l6") :- ()
      Copy("p3", "p") :- ()
      Copy("p3", "p2") :- ()
      Load("l6", "s", "p3") :- ()
      AddrOf("me", "e") :- ()
      Store("l7", "p3", "me") :- ()
      Load("l7", "t", "p3") :- ()
      CFG("l1", "l3") :- ()
      CFG("l3", "l5") :- ()
      CFG("l3", "l6") :- ()
      CFG("l5", "l6") :- ()
      CFG("l6", "l7") :- ()
    }

    program.api.solve()
    assert(program.Pt.numFacts() == 16)
  }

  def llvmTest(benchmarkName: String, ptCount: Int) = {
    test(benchmarkName) {
      val program = new SUProgram
      program.api.loadFactsFromFile(this.getClass.getResource("/"+benchmarkName+".flix").getPath, program.predicateNames)
      program.api.solve()
      assert(program.Pt.numFacts() == ptCount)
    }
  }
//  llvmTest("470.lbm", 469)
//  llvmTest("429.mcf", 1867)
//  llvmTest("458.sjeng", 4009)
}