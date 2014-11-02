#haspon

Haskell de pon

## 特徴
* パネルでポンが驚きの劣化
* アクティブ連鎖できない
* 色がついておらずわかりにくい

## 言語
Haskell

## 操作方法

### カーソルの移動
w : 上  
s : 下  
a : 左  
d : 右  

### パネルの操作
j : 左右のパネルを交換  
k : 並んだパネルを消す  
l : パネルを1行せり上げる  

## コンパイルと実行
* GHC 7.8.3 でコンパイル
* Mac 10.9.2 で動作を確認

### 引数
* 乱数のシード値を整数で指定
* 生成されるパネルが乱数によって決定される
* シード値が同じならば同じパネルが生成される

haspon 777

のように、シード値と一緒に起動する。
