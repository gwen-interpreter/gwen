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

import java.io.File
import org.mockito.Matchers.any
import org.mockito.Mockito.never
import org.mockito.Mockito.verify
import org.mockito.Mockito.when
import org.scalatest.Matchers
import org.scalatest.mock.MockitoSugar
import org.scalatest.FlatSpec

class GwenAppTest extends FlatSpec with Matchers with MockitoSugar {

  var ranREPL: Boolean = _ 
  
  private def createApp(options: GwenOptions, interpreter: GwenInterpreter[EnvContext]) = {
    ranREPL = false
    new GwenApp(interpreter) {
      override private[eval] def runRepl(env: EnvContext) {
        ranREPL = true
      }
    }
  }
  
  "Running app with no args" should "initialise env, execute options, run repl, and close env" in {
    
    val options = GwenOptions()
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val mockEnv = mock[EnvContext]
    val app = createApp(options, mockInterpreter)
    when(mockInterpreter.initialise(options)).thenReturn(mockEnv)
    
    app.run(options)
    
    verify(mockInterpreter).initialise(options);
    verify(mockInterpreter).execute(options, Some(mockEnv))
    verify(mockInterpreter).close(mockEnv)
    ranREPL should be (true)
  }
  
  "Running app with only batch option" should "not initialise env, execute options, not run repl, and not close env" in {
    
    val options = GwenOptions(batch = true)
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val app = createApp(options, mockInterpreter)
    
    app.run(options);
    
    verify(mockInterpreter, never()).initialise(options);
    verify(mockInterpreter).execute(options, None)
    verify(mockInterpreter, never()).close(any[EnvContext])
    ranREPL should be (false)
  }
  
  "Running interactive app with meta file" should "execute options and run repl" in {

    val options = GwenOptions(metaFile = Some(new File("file.meta")))
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val mockEnv = mock[EnvContext]
    val app = createApp(options, mockInterpreter)
    when(mockInterpreter.initialise(options)).thenReturn(mockEnv)
    
    app.run(options)
    
    verify(mockInterpreter).initialise(options);
    verify(mockInterpreter).execute(options, Some(mockEnv))
    verify(mockInterpreter).close(mockEnv)
    ranREPL should be (true)
  }
  
  "Running interactive app with feature file" should "execute options and run repl" in {

    val options = GwenOptions(paths = List(new File("file.feature")))
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val mockEnv = mock[EnvContext]
    val app = createApp(options, mockInterpreter)
    when(mockInterpreter.initialise(options)).thenReturn(mockEnv)
    
    app.run(options)
    
    verify(mockInterpreter).initialise(options);
    verify(mockInterpreter).execute(options, Some(mockEnv))
    verify(mockInterpreter).close(mockEnv)
    ranREPL should be (true)
  }

  "Running batch app with meta file" should "execute options and not run repl" in {
    
    val options = GwenOptions(batch = true, metaFile = Some(new File("file.meta")))
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val app = createApp(options, mockInterpreter)
    
    app.run(options)
    
    verify(mockInterpreter, never()).initialise(options);
    verify(mockInterpreter).execute(options, None)
    verify(mockInterpreter, never()).close(any[EnvContext])
    ranREPL should be (false)
    
  }
  
  "Running batch app with feature file" should "execute options and not run repl" in {
    
    val options = GwenOptions(batch = true, paths = List(new File("file.feature")))
    val mockInterpreter = mock[GwenInterpreter[EnvContext]]
    val app = createApp(options, mockInterpreter)
    
    app.run(options);
    
    verify(mockInterpreter, never()).initialise(options);
    verify(mockInterpreter).execute(options, None)
    verify(mockInterpreter, never()).close(any[EnvContext])
    ranREPL should be (false)
  }
  
}