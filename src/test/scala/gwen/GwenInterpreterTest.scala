/*
 * Copyright 2014 Branko Juric, Brady Wood
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package gwen

import gwen.core.GwenOptions
import gwen.core.eval.EvalContext
import gwen.core.eval.EvalEngine
import gwen.core.state.EnvState
import gwen.core.status._

import org.mockito.ArgumentMatchers.any
import org.mockito.ArgumentMatchers.same
import org.mockito.Mockito
import org.mockito.Mockito.doReturn
import org.mockito.Mockito.never
import org.mockito.Mockito.spy
import org.mockito.Mockito.verify
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar

import java.io.File
import gwen.core.eval.GwenREPL
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GwenInterpreterTest extends AnyFlatSpec with Matchers with MockitoSugar {

  private def createInterpreter(options: GwenOptions, engine: EvalEngine[EvalContext], repl: GwenREPL[EvalContext]) = {
    new GwenInterpreter(engine)
  }

  "Running app with no args" should "initialise env, execute options, run repl, and close env" in {

    val options = GwenOptions()
    val mockCtx = spy(new EvalContext(options, EnvState()))
    val mockRepl = mock[GwenREPL[EvalContext]]
    val mockEngine = Mockito.mock(classOf[EvalEngine[EvalContext]], Mockito.CALLS_REAL_METHODS)
    val interpreter = spy(createInterpreter(options, mockEngine, mockRepl))

    when(mockEngine.init(same(options), any[EnvState])).thenReturn(mockCtx)
    doReturn(Skipped).when(interpreter).run(options, Some(mockCtx))
    doReturn(mockRepl).when(interpreter).createRepl(mockCtx)

    interpreter.run(options) should be (0)

    verify(mockEngine).init(same(options), any[EnvState])
    verify(mockCtx).close()
    verify(mockRepl).run()
  }

  "Running app with only batch option" should "not initialise env, execute options, not run repl, and not close env" in {

    val options = GwenOptions(batch = true)
    val mockRepl = mock[GwenREPL[EvalContext]]
    val mockEngine = mock[EvalEngine[EvalContext]]
    val interpreter = spy(createInterpreter(options, mockEngine, mockRepl))

    doReturn(Skipped).when(interpreter).run(options, None)

    interpreter.run(options) should be (0)

    verify(mockEngine, never()).init(same(options), any[EnvState])
    verify(mockRepl, never()).run()
  }

  "Running interactive app with meta file" should "execute options and run repl" in {

    val options = GwenOptions(metas = List(new File("file.meta")))
    val mockCtx = spy(new EvalContext(options, EnvState()))
    val mockRepl = mock[GwenREPL[EvalContext]]
    val mockEngine = mock[EvalEngine[EvalContext]]
    val interpreter = spy(createInterpreter(options, mockEngine, mockRepl))

    when(mockEngine.init(same(options), any[EnvState])).thenReturn(mockCtx)
    doReturn(Passed(1)).when(interpreter).run(options, Some(mockCtx))
    doReturn(mockRepl).when(interpreter).createRepl(mockCtx)

    interpreter.run(options) should be (0)

    verify(mockEngine).init(same(options), any[EnvState])
    verify(mockCtx).close()
    verify(mockRepl).run()
  }

  "Running interactive app with feature file" should "execute options and run repl" in {

    val options = GwenOptions(features = List(new File("file.feature")))
    val mockCtx = spy(new EvalContext(options, EnvState()))
    val mockRepl = mock[GwenREPL[EvalContext]]
    val mockEngine = mock[EvalEngine[EvalContext]]
    val interpreter = spy(createInterpreter(options, mockEngine, mockRepl))

    when(mockEngine.init(same(options), any[EnvState])).thenReturn(mockCtx)
    doReturn(Passed(1)).when(interpreter).run(options, Some(mockCtx))
    doReturn(mockRepl).when(interpreter).createRepl(mockCtx)

    interpreter.run(options) should be (0)

    verify(mockEngine).init(same(options), any[EnvState])
    verify(mockCtx).close()
    verify(mockRepl).run()
  }

  "Running batch app with meta file" should "execute options and not run repl" in {

    val options = GwenOptions(batch = true, metas = List(new File("file.meta")))
    val mockRepl = mock[GwenREPL[EvalContext]]
    val mockEngine = mock[EvalEngine[EvalContext]]
    val interpreter = spy(createInterpreter(options, mockEngine, mockRepl))

    doReturn(Passed(1)).when(interpreter).run(options, None)

    interpreter.run(options) should be (0)

    verify(mockEngine, never()).init(same(options), any[EnvState])
    verify(mockRepl, never()).run()

  }

  "Running batch app with feature file" should "execute options and not run repl" in {

    val options = GwenOptions(batch = true, features = List(new File("file.feature")))
    val mockRepl = mock[GwenREPL[EvalContext]]
    val mockEngine = mock[EvalEngine[EvalContext]]
    val interpreter = spy(createInterpreter(options, mockEngine, mockRepl))

    doReturn(Passed(1)).when(interpreter).run(options, None)

    interpreter.run(options) should be (0)

    verify(mockEngine, never()).init(same(options), any[EnvState])
    verify(mockRepl, never()).run()
  }

}
