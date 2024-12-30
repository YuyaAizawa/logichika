# elm-github-pages

Elmで作成したSPAをGitHub Pagesで公開するときのテンプレート．
push時にGitHub Actionsを使って自動でコンパイル＆デプロイする．

## 使い方

1. このリポジトリの内容をコピー（git cloneでもforkしてrepository templateでも何でも）
1. 公開用のGitHubリポジトリの `Settings -> Pages -> Build and deployment -> Source` を **GitHub Actions** に設定
<img src="https://user-images.githubusercontent.com/7776405/188116490-7ce42ee4-ba2d-4147-9d7b-d7eeb49d4176.png" width="640px">

3.  `index.html` や `src/Main.js` を好きに編集（コンパイルしたコードは `js/elm.js` に出力されることに注意）
4. 公開するリポジトリに `push`
5. GitHub Actionsが処理するのを待つ
6. `Settings -> Pages` から出来を確認
<img src="https://user-images.githubusercontent.com/7776405/188117962-5216ab87-836c-4763-945f-3a0deb8120d9.png" width="640px">

7. よいElmライフを👍

![image](https://user-images.githubusercontent.com/7776405/188118448-d2873226-49ca-4811-8fad-87431678eb0d.png)

## ローカルで簡易的にデプロイ

```bash
elm make src/Main.elm --output=public/js/elm.js
```
して`index.html`を開く．  
※`public/js/elm.js`は`.gitignore`してある．

## その他

イシュー，プルリク歓迎！
