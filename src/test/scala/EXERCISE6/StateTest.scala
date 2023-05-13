package EXERCISE6

import EXERCISE6.Random._
import org.scalatest.concurrent.TimeLimits
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AnyFlatSpec


class StateTest extends AnyFlatSpec with Diagrams with TimeLimits {
  "map" should "ペア" in {
    val rng = SimpleRNG(123)

    // 状態と一緒に返ってくることだけ検証
    val state = State.unit[RNG, Int](10)
    val mappedState = state.map(_ * 2)
    val (result, _) = mappedState.run(rng)
    assert(result == 20)
  }

  "map2" should "ペア" in {
    val rng = SimpleRNG(123)
    val stateA = State.unit[RNG, Int](10)
    val stateB = State.unit[RNG, Int](20)
    val combinedState = stateA.map2(stateB)(_ + _)
    val (result, _) = combinedState.run(rng)
    assert(result == 30)
  }

  "Machine" should "動かす" in {
    // ロック状態、キャンディ5個、コイン10枚
    val machine = Machine(true, 5, 10)
    // 4回キャンディを購入する
    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    val result = 自動販売機.simulateMachine(inputs).run(machine)
    // コイン14枚、キャンディ1個になる
    assert(result._1 == (14, 1))
  }
}










