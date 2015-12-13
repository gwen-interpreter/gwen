publishMavenStyle := true

pomIncludeRepository := { _ => false }

publishArtifact in Test := false

pomExtra := (
  <scm>
    <connection>scm:git:git@github.com:gwen-interpreter/gwen.git</connection>
    <developerConnection>scm:git:git@github.com:gwen-interpreter/gwen.git</developerConnection>
    <url>git@github.com:gwen-interpreter/gwen.git</url>
  </scm>
  <developers>
    <developer>
      <id>bjuric</id>
      <name>Branko Juric</name>
      <url>https://github.com/bjuric</url>
    </developer>
    <developer>
      <id>bradywood</id>
      <name>Brady Wood</name>
      <url>https://github.com/bradywood</url>
    </developer>
  </developers>)