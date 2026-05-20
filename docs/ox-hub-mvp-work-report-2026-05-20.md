# ox-hub MVP Work Report

作成日: 2026-05-20

## Summary

MVP までの実装として、Org から Zenn / Qiita 向け Markdown を生成する基礎機能は一通り揃っている。

直近では、MVP 前レビューで見つかった Markdown 出力の崩れやすい箇所を修正した。

- コードブロック本文に ``` が含まれる場合でも Markdown が壊れないようにした。
- ネストしたリストを余計な空白行なしで出力できるようにした。
- `batch-byte-compile` の unused argument warning を解消した。

実装コミット確認時点の作業ツリーは clean。byte-compile で生成される `.elc` は `.gitignore` 対象。

## Recent Commits

```text
556f9a9 fix: refine nested list
b8a0fd8 fix: extend markdown code fences when needed
a05a6a6 docs: make Japanse README and add Installation, etc.
e831a35 feat: render oxhub directives
fbfe72b feat: add markdown export commands
4715a50 feat: render markdown body
a0d3a0e feat: add new article command
937f0ca feat: render front matter
```

## Completed Work

### Core MVP Implementation

- Org metadata extraction and validation.
- Slug validation.
- New article command.
- Zenn / Qiita front matter rendering.
- Markdown body rendering.
- Export commands for Zenn, Qiita, and both targets.
- `oxhub` special block directives.
  - `message`
  - `details`
  - `codefile`

### Documentation

- `README.org` を英語版 README として整備。
- `README.ja.org` を日本語版 README として整備。
- Installation と End-to-end Example を追加。
- 使用技術 shield を追加。
- 対応済み記法と制限事項を README に整理。

### Markdown Rendering Fixes

#### Code Fence

コードブロック本文に三連バッククォートが含まれる場合、従来は固定の ``` フェンスで囲んでいたため Markdown が途中で閉じていた。

修正後は、本文中の最大バッククォート連続長を調べ、それより長いフェンスを使う。

```text
本文に ``` が含まれる場合: 外側は ````
本文に ```` が含まれる場合: 外側は `````
```

対象:

- `#+begin_src`
- `#+begin_example`
- `#+begin_oxhub codefile`

#### Nested List

ネストしたリストで、子リストの前に空白だけの行が出ていた。

修正後は、list item 直下の要素を構造ベースで扱い、通常本文と子 `plain-list` を分けて組み立てる。

期待出力:

```markdown
- parent
  - child
```

対応済みケース:

- unordered list のネスト。
- ordered list のネスト。
- unordered 直下の ordered list。
- 3 階層以上のネスト。

## Verification

直近確認時点の ERT:

```sh
emacs --batch -l ert -l ox-hub.el -l test/ox-hub-test.el -f ert-run-tests-batch-and-exit
```

結果:

```text
Ran 66 tests, 66 results as expected, 0 unexpected
```

byte-compile:

```sh
emacs --batch -Q -L . -f batch-byte-compile ox-hub.el test/ox-hub-test.el
```

結果:

```text
warning / error なし
```

## Next Work Proposal

### 1. Qiita details summary の HTML エスケープ

次に優先するならこれが最も自然。

Qiita 向け `details` は以下のように raw HTML を出力している。

```html
<details><summary>Summary text</summary>

Body

</details>
```

現状では summary をそのまま HTML に埋め込んでいるため、summary に `<`, `>`, `&`, `"` が含まれると HTML が崩れる可能性がある。

対応案:

- Qiita の `summary` だけ HTML エスケープする。
- Zenn の `:::details` 出力は現状維持する。
- `&`, `<`, `>`, `"` の変換テストを追加する。

想定コミットメッセージ:

```text
fix: escape qiita details summary
```

### 2. 複数段落 list item のテスト固定

ネストリスト対応で list item の組み立てを構造ベースに変更したため、複数段落を含む item の期待出力もテストで固定しておくと安心。

例:

```org
- first paragraph

  second paragraph
```

このケースは不具合修正というより、今回の周辺保証として扱う。

想定コミットメッセージ:

```text
test: cover multi-paragraph list items
```

### 3. README の対応記法一覧の微調整

コードフェンス自動延長やネストリスト対応は内部品質寄りの改善だが、README の対応記法一覧で「リスト」の対応範囲を少し明確にしてもよい。

対応案:

- 共通 Markdown のリスト項目に nested list 対応を明記する。
- 制限事項に残っている未対応記法と区別できるようにする。

想定コミットメッセージ:

```text
docs: clarify supported list rendering
```

## Recommended Next Step

次回は `Qiita details summary の HTML エスケープ` から着手する。

理由:

- MVP 前レビューで残っている明確な出力不具合候補。
- 実装範囲が小さい。
- Zenn / Qiita の target-specific behavior として整理しやすい。
- テストで期待値を固定しやすい。
