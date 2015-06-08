Attribute VB_Name = "mod_drawtreestructure"

'使い方
'階層構造的値が含まれる罫線を引きたいセルを選択する
'イミディエイトなどで「DRAW_TREE_STREUCTURE」を実行する
'今後のFeature かつ 現状仕様
'※1 異常系（セルが選択されていない、複数選択セルの場合など）に対応できていないので対応する
'※2 同じ値が同列にあった場合は、同列とみなされるので、同列と見なさない対応をする
'※3 すでに罫線が引かれている場合は特に対応はしないので、この罫線を削除する対応をする
'
'不具合
'※1 第一階層、第二階層と、階層が続く場所に同じ値が格納されている場合に罫線の引かれ方がおかしい、階層を判定する処理が必要かもしれない
'
'メリット
' 
'※1 階層構造をパースして構造をモデリングしてから罫線を引くというような形ではないので、あまりきれいなコードではなく汎用性はない、ただし二次元配列的にあつかっているのでそれと比較して速く罫線が引ける（要測定必要）
'※2 スクラッチで自分で書いていますので、不要なライブラリ等
' 
'その他

Public Sub DRAW_TREE_STRUCTURE()
    Dim xlen As Integer
    Dim ylen As Integer
    Dim baseaddr As String
    baseaddr = Selection.Cells(1, 1).Address
    xlen = Selection.Columns.Count
    ylen = Selection.Rows.Count
    
    Dim tx As Integer
    Dim ty As Integer
    
    Dim rs As Integer '開始
    Dim re As Integer '終了
    Dim iv As String  '対象の項目値
    Dim cR As Range   'カレントセル
    
    'レベルごと
    For tx = 0 To xlen - 1
        For ty = 0 To ylen - 1
            '各レベル毎に罫線を引く
            'Debug.Print range(baseaddr).Offset(tx, ty).Value, tx, ty
            '値があり初回の行ではない場合に、それを一つの区切りとする
            Set cR = Range(Range(baseaddr).Offset(ty, tx).Address)
            '初回行の場合
            If ty = 0 Then
                If cR.Value <> "" Then
                    iv = cR.Value
                    rs = ty
                End If
            End If
            '次回行の場合
            If cR.Value <> "" And iv <> cR.Value Then
                iv = cR.Value
                '末尾でcommit
                Range(cR.Offset(rs - ty, 0).Address & ":" & cR.Offset(-1, xlen - tx - 1).Address).BorderAround LineStyle:=xlContinuous, Weight:=xlThin
                rs = ty
            End If
            '最終行の場合
            If ty = ylen - 1 Then
                '末尾でcommit
                Range(cR.Offset(rs - ty, 0).Address & ":" & cR.Offset(0, xlen - tx - 1).Address).BorderAround LineStyle:=xlContinuous, Weight:=xlThin
            End If
        Next ty
    Next tx
End Sub
