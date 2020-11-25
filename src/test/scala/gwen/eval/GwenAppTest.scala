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

package gwen.eval

import gwen.dsl.Passed

import org.mockito.Mockito.never
import org.mockito.Mockito.verify
import org.mockito.Mockito.when
import org.scalatest.Matchers
import org.scalatestplus.mockito.MockitoSugar
import org.scalatest.FlatSpec

import java.io.File

class GwenAppTest extends FlatSpec with Matchers with MockitoSugar {

  private def createApp(options: GwenOptions, interpreter: GwenInterpreter[EnvContext], repl: GwenREPL[EnvContext]) = {
    when(interpreter.implName).thenReturn("gwen")
    when(interpreter.implVersion).thenReturn("-SNAPSHOT")
    when(interpreter.noticeMsg).thenReturn(None)
    new GwenApp(interpreter) {
      override private[eval] def createRepl(env: EnvContext): GwenREPL[EnvContext] = repl
    }
  }
  
  "Running app with no args" should "initialise env, execute options, run repl, and close env" in {
    
    val options = GwenOptions()
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val mockLauncher = mock[GwenLauncher[EnvContext]]
    val mockEnv = mock[EnvContext]
    val mockRepl = mock[GwenREPL[EnvContext]]
    val app = createApp(options, mockInterpreter, mockRepl)
    when(mockEnv.loadedMeta).thenReturn(Nil)
    when(mockInterpreter.initialise(options)).thenReturn(mockEnv)
    when(mockLauncher.run(options, Some(mockEnv))).thenReturn(Passed(1))
    
    app.run(options)(mockLauncher) should be (0)
    
    verify(mockInterpreter).initialise(options)
    verify(mockEnv).close()
    verify(mockRepl).run()
  }
  
  "Running app with only batch option" should "not initialise env, execute options, not run repl, and not close env" in {
    
    val options = GwenOptions(batch = true)
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val mockLauncher = mock[GwenLauncher[EnvContext]]
    val mockRepl = mock[GwenREPL[EnvContext]]
    val app = createApp(options, mockInterpreter, mockRepl)
    
    when(mockLauncher.run(options, None)).thenReturn(Passed(1))
    
    app.run(options)(mockLauncher) should be (0)
    
    verify(mockInterpreter, never()).initialise(options)
    verify(mockRepl, never()).run()
  }
  
  "Running interactive app with meta file" should "execute options and run repl" in {

    val options = GwenOptions(metas = List(new File("file.meta")))
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val mockLauncher = mock[GwenLauncher[EnvContext]]
    val mockEnv = mock[EnvContext]
    val mockRepl = mock[GwenREPL[EnvContext]]
    val app = createApp(options, mockInterpreter, mockRepl)
    when(mockEnv.loadedMeta).thenReturn(Nil)
    when(mockInterpreter.initialise(options)).thenReturn(mockEnv)
    when(mockLauncher.run(options, Some(mockEnv))).thenReturn(Passed(1))
    
    app.run(options)(mockLauncher) should be (0)
    
    verify(mockInterpreter).initialise(options)
    verify(mockEnv).close()
    verify(mockRepl).run()
  }
  
  "Running interactive app with feature file" should "execute options and run repl" in {

    val options = GwenOptions(features = List(new File("file.feature")))
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val mockLauncher = mock[GwenLauncher[EnvContext]]
    val mockEnv = mock[EnvContext]
    val mockRepl = mock[GwenREPL[EnvContext]]
    val app = createApp(options, mockInterpreter, mockRepl)
    when(mockInterpreter.initialise(options)).thenReturn(mockEnv)
    when(mockLauncher.run(options, Some(mockEnv))).thenReturn(Passed(1))
    
    app.run(options)(mockLauncher) should be (0)
    
    verify(mockInterpreter).initialise(options)
    verify(mockEnv).close()
    verify(mockRepl).run()
  }

  "Running batch app with meta file" should "execute options and not run repl" in {
    
    val options = GwenOptions(batch = true, metas = List(new File("file.meta")))
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val mockLauncher = mock[GwenLauncher[EnvContext]]
    val mockRepl = mock[GwenREPL[EnvContext]]
    val app = createApp(options, mockInterpreter, mockRepl)
    when(mockLauncher.run(options, None)).thenReturn(Passed(1))
    
    app.run(options)(mockLauncher) should be (0)
    
    verify(mockInterpreter, never()).initialise(options)
    verify(mockRepl, never()).run()
    
  }
  
  "Running batch app with feature file" should "execute options and not run repl" in {
    
    val options = GwenOptions(batch = true, features = List(new File("file.feature")))
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val mockLauncher = mock[GwenLauncher[EnvContext]]
    val mockRepl = mock[GwenREPL[EnvContext]]
    val app = createApp(options, mockInterpreter, mockRepl)
    
    when(mockLauncher.run(options, None)).thenReturn(Passed(1))
    
    app.run(options)(mockLauncher) should be (0)
    
    verify(mockInterpreter, never()).initialise(options)
    verify(mockRepl, never()).run()
  }
  
}