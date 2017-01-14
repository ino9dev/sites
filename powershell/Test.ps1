function Get-TestDataFile {
    <#
    .DESCRIPTION test
    #>
    param(
        [Parameter(ValueFromPipeline=$true)]
        [int32]$Depth=1,
        [int32]$Level=1,
        [int32]$Size=(1024*1024*44), #44Mbyte
        [int32]$Filecount=5,
        [String]$Name="",
        [String]$LevelFolderName="l",
        [String]$DepthFolderName="d",
        [String]$Basechar="0", #1byte
        [String]$Basefilename="test",
        [String]$Value=$Basechar*$Size
    )
    begin{

        # 起点とするパスはTest.ps1の実行パス（事故防止仕様）
        $BasePath = (Split-Path ( & { $myInvocation.ScriptName } ) -parent)

        # 名前が指定されていたら、それも起点に含める
        if($Name -ne ""){$BasePath += "/$Name"}
 
        # データの元になるデータファイルの一時保存域を作成する
        $date = (Get-Date).ToString("yyyyMMddhhmmss")
        $tmpfolder = "$BasePath\tmp_$date"
        md $tmpfolder | Out-Null
        
        # データの元になるデータファイルを作成する
        ni -Type file -Force -Value $Value "$tmpfolder\$Basefilename"

        # ファイルセット作成
        (1..($Filecount-1)) | % {
            cp "$tmpfolder\$Basefilename" "$tmpfolder\$Basefilename$_" | Out-Null
        }
    }

    #testcase
    # depth=3;filecount=100;
    # 3階層(,1水平)に100ファイル
    # a1/a2/a3(100)

    #testcase
    # depthの上で、level
    # 3階層,2水平に100ファイル
    # a1/a2/a3 (100)
    # b1/b2/b3 (100)
        
    #testcase
    # 1階層,1水平に100ファイル
    # a (100)

    # 1階層,3水平に100ファイル
    # a (100)
    # b (100)
    # c (100)
    process{

        #todo 指定の値で埋める（文字列的に）
        #todo byte単位で作成できるようにする

        (1..$Level) | % {
            $tLevel = "$LevelFolderName$_"
            $tDepth = ""
            (1..$Depth) | % {
                $tDepth = $tDepth + "/$DepthFolderName$_"
            }
            if(Test-Path -path "$BasePath/$tLevel/$tDepth"){rd -Force -Recurse "$BasePath/$tLevel/$tDepth"}
            md -Force "$BasePath/$tLevel/$tDepth" |Out-Null
            # copy fileset to tDepth
            cp $tmpfolder/* "$BasePath/$tLevel/$tDepth"
        }
    }
    end{
        rd -Recurse -Force $tmpfolder
        ls $Path
    }
}

#Get-TestDataFile -Depth 5
#Get-TestDataFile -Filecount 100
#Get-TestDataFile -Depth 1 -Level 1 -Name "tc1"
#Get-TestDataFile -Depth 1 -Level 2 -Path "testcase2"
#Get-TestDataFile -Depth 2 -Level 1 -Path "testcase3"

function Get-TestImageFile {
    <#
    .DESCRIPTION test
    #>
    param(
        [Parameter(ValueFromPipeline=$true)]
        [int32]$Depth=1
        ,[int32]$Level=1
        ,[System.Int32[][]]$Sizes=(,(1024,768)) #XGA
        ,[int32]$Filecount=5
        ,[String]$Basefilename="img"
        ,[String]$Name=""
    )
    begin{
        # todo 実行パスにファイルを作成する
        # 画像データ生成用のクラスを読み込む
        Add-Type -AssemblyName System.Drawing

        # 起点とするパスはTest.ps1の実行パス（事故防止仕様）
        $BasePath = (Split-Path ( & { $myInvocation.ScriptName } ) -parent)
        
        # 名前が指定されていたら、それも起点に含める、そのフォルダを作成しておく
        if($Name -ne ""){$BasePath += "\$Name";md $BasePath|Out-null}
    }
    process{

        # 指定のサイズ分画像データを生成する
        $Sizes | % {
            ($x_size,$y_size) = ($_[0],$_[1])
            $bmp = new-object System.Drawing.Bitmap($x_size,$y_size)
            $font = new-object System.Drawing.Font Consolas,24 
            $brushBg = [System.Drawing.Brushes]::Yellow 
            $brushFg = [System.Drawing.Brushes]::Black 
            $pens = [System.Drawing.Pens]::Red
            $graphics = [System.Drawing.Graphics]::FromImage($bmp) 
            $graphics.FillRectangle($brushBg,0,0,$bmp.Width,$bmp.Height)
            $graphics.DrawEllipse($pens, 0, 0, 300, 300)
            $graphics.DrawString('HELLO TEST DATA',$font,$brushFg,0,150)
            $graphics.Dispose() 
            if(Test-Path -path "$BasePath\$Basefilename$x_size-$y_size.png"){
                rm "$BasePath\$Basefilename$x_size-$y_size.png"
            }
            $bmp.Save("$BasePath\$Basefilename$x_size-$y_size.png")
        }

        # todo : 指定の色のファイルを生成する
        $Colors | % {
            # todo 指定の色のrectangleファイルを生成する
            # colors-optionを指定する
        }
    }
    end{
    }
}

# 1024
Get-TestImageFile -Name "tc1"
# QVGA,VGA,SVGA
Get-TestImageFile -Name "tc2" -Sizes ((320,240),(640,480),(800,600))
# Get-TestImageFile 
# Get-TestImageFile 