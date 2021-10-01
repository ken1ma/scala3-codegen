  /*
    [Compiler phases definition](https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/dotc/Compiler.scala)
    [Compiler phases doc](https://dotty.epfl.ch/docs/internals/overall-structure.html)

    As of Scala 3.0.2, `scalac -Xshow-phases` shows
    
      typer
      inlinedPositions
      sbt-deps
      extractSemanticDB
      posttyper
      prepjsinterop
      sbt-api
      SetRootTree
      pickler
      inlining
      postInlining
      staging
      pickleQuotes
      {firstTransform, checkReentrant, elimPackagePrefixes, cookComments, checkStatic, betaReduce, inlineVals, expandSAMs, initChecker}
      {elimRepeated, protectedAccessors, extmethods, uncacheGivenAliases, byNameClosures, hoistSuperArgs, specializeApplyMethods, refchecks}
      {elimOpaque, tryCatchPatterns, patternMatcher, explicitJSClasses, explicitOuter, explicitSelf, elimByName, stringInterpolatorOpt}
      {pruneErasedDefs, uninitializedDefs, inlinePatterns, vcInlineMethods, seqLiterals, intercepted, getters, specializeFunctions, liftTry, collectNullableFields, elimOuterSelect, resolveSuper, functionXXLForwarders, paramForwarding, genericTuples, letOverApply, arrayConstructors}
      erasure
      {elimErasedValueType, pureStats, vcElideAllocations, arrayApply, addLocalJSFakeNews, elimPolyFunction, tailrec, completeJavaEnums, mixin, lazyVals, memoize, nonLocalReturns, capturedVars}
      {constructors, instrumentation}
      {lambdaLift, elimStaticThis, countOuterAccesses}
      {dropOuterAccessors, checkNoSuperThis, flatten, renameLifted, transformWildcards, moveStatic, expandPrivate, restoreScopes, selectStatic, junitBootstrappers, collectSuperCalls, repeatableAnnotations}
      genSJSIR
      genBCode

    provided the following setup

      # https://get-coursier.io/docs/cli-installation
      curl -fLo /usr/local/bin/cs https://git.io/coursier-cli-"$(uname | tr LD ld)"
      chmod +x /usr/local/bin/cs

      cs install scala3-compiler
      export PATH="$HOME/Library/Application Support/Coursier/bin:$PATH"
      scala3-compiler -Xshow-phases

    which is a lot less than [that of Scala 2](https://typelevel.org/scala/docs/phases.html)
  */
