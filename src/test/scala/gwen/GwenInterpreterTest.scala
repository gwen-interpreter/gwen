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

import gwen.eval.EvalContext
import gwen.eval.EvalEngine
import gwen.eval.EvalEnvironment
import gwen.eval.GwenLauncher
import gwen.eval.GwenREPL
import gwen.model.Passed
import gwen.model.Skipped

import org.mockito.Mockito.never
import org.mockito.Mockito.spy
import org.mockito.Mockito.verify
import org.mockito.Mockito.when
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatestplus.mockito.MockitoSugar

import java.io.File

class GwenInterpreterTest extends FlatSpec with Matchers with MockitoSugar {

  private def createApp(options: GwenOptions, engine: EvalEngine[EvalContext], repl: GwenREPL[EvalContext]) = {
    new GwenInterpreter(engine) {
      override private[gwen] def createRepl(ctx: EvalContext): GwenREPL[EvalContext] = repl
    }
  }
  
  "Running app with no args" should "initialise env, execute options, run repl, and close env" in {
    
    val options = GwenOptions()
    val mockLauncher = mock[GwenLauncher[EvalContext]]
    val mockEnv = mock[EvalEnvironment]
    val mockCtx = spy(new EvalContext(options, mockEnv))
    val mockRepl = mock[GwenREPL[EvalContext]]
    val mockEngine = mock[EvalEngine[EvalContext]]
    val app = createApp(options, mockEngine, mockRepl)

    when(mockEngine.init(options, None)).thenReturn(mockCtx)
    when(mockLauncher.run(options, Some(mockCtx))).thenReturn(Skipped)
    
    app.run(options, mockLauncher) should be (0)
    
    verify(mockEngine).init(options, None)
    verify(mockCtx).close()
    verify(mockRepl).run()
  }
  
  "Running app with only batch option" should "not initialise env, execute options, not run repl, and not close env" in {
    
    val options = GwenOptions(batch = true)
    val mockLauncher = mock[GwenLauncher[EvalContext]]
    val mockRepl = mock[GwenREPL[EvalContext]]
    val mockEngine = mock[EvalEngine[EvalContext]]
    val app = createApp(options, mockEngine, mockRepl)
    
    when(mockLauncher.run(options, None)).thenReturn(Skipped)
    
    app.run(options, mockLauncher) should be (0)
    
    verify(mockEngine, never()).init(options, None)
    verify(mockRepl, never()).run()
  }
  
  "Running interactive app with meta file" should "execute options and run repl" in {

    val options = GwenOptions(metas = List(new File("file.meta")))
    val mockLauncher = mock[GwenLauncher[EvalContext]]
    val mockEnv = mock[EvalEnvironment]
    val mockCtx = spy(new EvalContext(options, mockEnv))
    val mockRepl = mock[GwenREPL[EvalContext]]
    val mockEngine = mock[EvalEngine[EvalContext]]
    val app = createApp(options, mockEngine, mockRepl)

    when(mockEngine.init(options, None)).thenReturn(mockCtx)
    when(mockLauncher.run(options, Some(mockCtx))).thenReturn(Passed(1))
    
    app.run(options, mockLauncher) should be (0)
    
    verify(mockEngine).init(options, None)
    verify(mockCtx).close()
    verify(mockRepl).run()
  }
  
  "Running interactive app with feature file" should "execute options and run repl" in {

    val options = GwenOptions(features = List(new File("file.feature")))
    val mockLauncher = mock[GwenLauncher[EvalContext]]
    val mockEnv = mock[EvalEnvironment]
    val mockCtx = spy(new EvalContext(options, mockEnv))
    val mockRepl = mock[GwenREPL[EvalContext]]
    val mockEngine = mock[EvalEngine[EvalContext]]
    val app = createApp(options, mockEngine, mockRepl)

    when(mockEngine.init(options, None)).thenReturn(mockCtx)
    when(mockLauncher.run(options, Some(mockCtx))).thenReturn(Passed(1))
    
    app.run(options, mockLauncher) should be (0)
    
    verify(mockEngine).init(options, None)
    verify(mockCtx).close()
    verify(mockRepl).run()
  }

  "Running batch app with meta file" should "execute options and not run repl" in {
    
    val options = GwenOptions(batch = true, metas = List(new File("file.meta")))
    val mockLauncher = mock[GwenLauncher[EvalContext]]
    val mockRepl = mock[GwenREPL[EvalContext]]
    val mockEngine = mock[EvalEngine[EvalContext]]
    val app = createApp(options, mockEngine, mockRepl)

    when(mockLauncher.run(options, None)).thenReturn(Passed(1))
    
    app.run(options, mockLauncher) should be (0)
    
    verify(mockEngine, never()).init(options, None)
    verify(mockRepl, never()).run()
    
  }
  
  "Running batch app with feature file" should "execute options and not run repl" in {
    
    val options = GwenOptions(batch = true, features = List(new File("file.feature")))
    val mockLauncher = mock[GwenLauncher[EvalContext]]
    val mockRepl = mock[GwenREPL[EvalContext]]
    val mockEngine = mock[EvalEngine[EvalContext]]
    val app = createApp(options, mockEngine, mockRepl)
    
    when(mockLauncher.run(options, None)).thenReturn(Passed(1))
    
    app.run(options, mockLauncher) should be (0)
    
    verify(mockEngine, never()).init(options, None)
    verify(mockRepl, never()).run()
  }
  
}