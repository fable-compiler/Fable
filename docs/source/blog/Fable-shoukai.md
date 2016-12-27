 - tagline: どこでもF#を使いましょう！

# Fable(F#からJSへコンパイラー)の紹介

皆さま、スペインからこんにちは！これは日本語の「[F# Advent Calendar 2016](http://qiita.com/advent-calendar/2016/fsharp)」
の2５日目の記事です、一日遅れましたがご了承ください(;^_^A ｱｾｱｾ･･･

----------------

今回もしつこくFableという自分のプロジェクトについて話したいです。私のプログラミングの経験は
大体.NET/C#,２年前に同時に関数プログラミングとWebプログラミングを始めて、F#言語に憧れて
F#をウェブに対応できる方法を探しまして、その時[FunScript](http://funscript.info/)を見つかりました。
FunScriptはF＃のソースコードをJSへコンパイルし、とても面白いプロジェクトでした。それがきっかけで
初めて私のオープンソースの経験としては下手でしたがFunScriptに色々な貢献をしてみました。２０１５年にも
[鈴木さん](https://twitter.com/yukitos)のおかげで[東京のF＃グループの前でFunScriptについて喋る
機会がありました](https://www.youtube.com/watch?v=c7z7b-0dkjo&t=602s)。

残念ながらFunScriptもいろいろな限度がありましたので大きいアプリを開発するにはあまりふさわしいツールでは
なかったです。去年の終わりにF＃でもJS側でもソースコードではなくAST([Abstract Syntax Tree](https://ja.wikipedia.org/wiki/%E6%8A%BD%E8%B1%A1%E6%A7%8B%E6%96%87%E6%9C%A8))
を読んだり書いたりなどのツールができましたので（[F# Compiler Services](http://fsharp.github.io/FSharp.Compiler.Service/)と[Babel](https://babeljs.io/))
ゼロから新しいコンパイラー作るチャンスだと思いました。最初は単純にただBabelにただ文字を変えて「Fabel」
の名前でしたが、F＃のコミュニティの意見を聞いたら、やっぱり「Fable」はもっと似合う名前でした。

新しいコンパイラーを作るにはどのメリットがあるかと聞かれたら…

- 直接F＃ソースコードを読みますので、JSへ変更する時間は早くなります（特に`--watch`モードにて）
- [ES2015](https://babeljs.io/learn-es2015/)を目的としていますが、Babelを通じて古いブラウザでも実行できます
- F＃のファイルは[ES2015 モジュールモジュール](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Statements/import)に
  なりますので、JSとの相互運用は高くなります
- ウェブだけではなく、JSの実行できるところ（デスクトップやモバイルなど）のターゲットもできます

Fableの初めてのコミットしてからもう一年になりますが、その間F＃コミュニティの応援のおかげで大きなプロジェクトに
なりました。もうFableで制作された[The Gamma Project](http://thegamma.net/)のような大きなプロジェクトウェブは
発表されています。まだ英語だけですが、皆さん興味がありましたら、[Fableのホームサイト](http://fable.io/)と[サンプル](http://fable.io/samples.html)
をチエックしてください。そして質問がありましたらご気軽に[連絡をお願いします]()。Fableを使って皆さんがすごいアプリを
作ることを期待しています！
