# ベーシックなデータ定義とオペレータ操作
# range operatorを使う
(1..10) | % {Write $_}
(1..10) | ForEach {Write $_} # alias1
(1..10) | ForEach-Object {Write $_} # alias2

# 配列を使う
(1,2,3) | % {Write $_}

# 連想配列（ハッシュテーブル）を使う
# @{k1=v1,k2=v2) this is defined as [hashtable]
@{"a"=1;"b"=2}.keys | % {Write $_} # list of names
@{"a"=1;"b"=2}.values | % {Write $_} # list of values
 
# Todo 直接操作すると操作できない件の調査、オブジェクトの型が違うのか、
$h = @{"a"=1};$h.Add("b",2);Write $h

# オブジェクト指向＋プロトタイプ
# TODO 前述のメンバー変数へアクセスするための方法this.Name的な表現が上手く通らない
#| Add-Member -MemberType ScriptMethod -Name say -Value {"hello"+Get-Member($This.Name)} -PassThru
$MamalClass = {$MyInvocation.MyCommand.ScriptBlock `
    | Add-Member -PassThru -MemberType NoteProperty -Name Name -Value "mamal" `
    | Add-Member -MemberType ScriptMethod -Name hello -Value {"hello"} -PassThru
}
$obj = &$MamalClass
$obj.hello()

# 日付系
(Get-Date).ToString("yyyyMMddhhmmss")

# snipet for Function and Commandlet
function CommandletName {
    param(
        [Parameter(ValueFromPipeline=$true,Mandatory=$true)]
        #[type]paramname,
        #[type]paramname,
        #[type]paramname
    )
    begin{}
    #main process
    process{}
    end{}
}

############################################################
#
# データ生成系
#
############################################################
function Get-TestDataFile {
    <#
    .DESCRIPTION
    
    特定のサイズのファイルを生成する
    
    .EXAMPLE
    
    Get-TestDataFile -Filecount 10
    Get-TestDataFile -Depth 5
    Get-TestDataFile -Depth 1 -Level 2 -Path "testcase2"
    Get-TestDataFile -Depth 2 -Level 1 -Path "testcase3"
     
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

# urlencodeを行う
# デフォルトではライブラリがロードされていないのでしておく
# output : http%3a%2f%2ftest.com%2fgetparam%3d!%23%24%25%26%27(
Add-Type -AssemblyName System.Web
[System.Web.HttpUtility]::UrlEncode("http://test.com/getparam=!#$%&'(")

# urldecodeを行う
# output : http://test.com/getparam=!#$%&'(
[System.Web.HttpUtility]::UrlDecode("http%3a%2f%2ftest.com%2fgetparam%3d!%23%24%25%26%27(")

# 文字列をbase64にencodeする
Write-Host -NoNewline [System.Convert]::ToBase64String([System.Text.Encoding]::UTF8.GetBytes("TEST!#$%&'()"))

# basic認証用の文字列を生成する
# username, password, basic authentication stringで出力する
function Get-BasicAuth
{
    param(
        [Parameter(ValueFromPipeline=$true,Mandatory=$true)]
        [String]$Username="ino9dev",
        [String]$Password="ino9dev's password"
    )
    begin{}
    process{
        $authbase = $Username + ":" + $Password;
        $basic = [System.Convert]::ToBase64String([System.Text.Encoding]::UTF8.GetBytes($authbase));
        $basic;
    }
    end{}
}
Get-BasicAuth -Username "ino9dev" -Password "ino9dev's password"

# ランダムな文字列を生成する（charsetを与えて10文字出力）
function Get-RandomStr
{
    param(
        [Parameter(ValueFromPipeline=$true,Mandatory=$true)]
        [int32] $Count = 10,
        [String]$Charset = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    )
    begin{}
    process{
        $result = (1..$Count) | % {(Get-Random $Charset.ToCharArray() -Count 1)}
        (-join $result)
    }
    end{
    }
}
Get-RandomStr -Count 100

# 規則的な複数ユーザID,パスワードを生成する（USER001-USER010）
(0..10) | % {Write ($_.ToString("USER000") +","+ $_.ToString("PASS000"))}

# 複数ユーザ分（パスワードはPASSWORD）のbasic認証文字列を生成する(10ユーザ分） 
# 前述のGet-BasicAuthを使用する
(0..10) | % {Get-BasicAuth -Username $_.ToString("USER000") -Password $_.ToString("PASS000")}

# 1 dimensional simple random walk
# 無限回繰り返した場合に、点がある位置に存在する確率は正規分布で示される。
# 以下の例では、100回のコイントスを10000回試行している
# depend on function Get-RandomStr

# ((0..99) | % {Get-RandomStr -Charset "01" -Count 1} | % {switch($_){"a"{-1} "b"{+1}}} | Measure -Sum |select -ExpandProperty Sum

(1..100) | % {(1..100) | % {Get-RandomStr -Charset "01" -Count 1} | % {switch($_){"0"{-1} "1"{+1}}} | Measure -Sum | % {echo "$_.Sum"}}

############################################################
#
# ファイルシステム系
#
############################################################
# あるフォルダ配下のフォルダのみ検索
Get-ChildItem -Path . -Recurse

# ある更新日付のファイルのみ検索

# あるサイズのファイルのみ検索

############################################################
#
# テスト用ツールダウンロード
#
############################################################
function DownloadTestTools {
    <#
    .DESCRIPTION テスト用のツールをダウンロードします。テストタイプによって、ダウンロードするツール群を指定します
    .NOTES もしこのfunctionを使用する場合、DL先URLは、適切に、各自ようにごご変更ください
    .EXAMPLE DownloadTestTools
    #>
    param(
        [Parameter(ValueFromPipeline=$true)]
        [String]$Today=((Get-Date).ToString("yyyyMMddhhmmss")),
        [String]$Dirname="Download_$Today",
        [String]$BaseDir=".",
        [String]$Path="$BaseDir\\$Dirname",
        [String[]]$Target=("LT","COMMON"),
        [HashTable]$ToolsGroup=@{
            "COMMON"=@{
                "eclipse-modeling-mars-R-win32-x86_64.zip"="http://www.eclipse.org/downloads/download.php?file=/technology/epp/downloads/release/mars/R/eclipse-modeling-mars-R-win32-x86_64.zip";
                "AtomSetup.exe"="https://atom.io/download/windows";
                "WinSCP"="";
                "BZ"="";
                "FireFox Setup Stub 38.05"="https://download.mozilla.org/?product=firefox-stub&os=win&lang=ja";
                "teraterm-4.87.exe"="http://osdn.jp/projects/ttssh2/downloads/63335/teraterm-4.87.exe/";
            };
            "LT"=@{
                "apache-jmeter-2.13.zip"="http://jmeter.apache.org/software/apache//jmeter/binaries/apache-jmeter-2.13.zip";
                "fiddler-4setup.exe"="http://www.telerik.com/download/fiddler/fiddler2";
                "fiddler-2setup.exe"="http://www.telerik.com/download/fiddler/fiddler4";
                "WinMerge-2.14.0-jp-80-x64-Setup.exe.zip"="http://www.geocities.co.jp/SiliconValley-SanJose/8165/WinMerge-2.14.0-jp-80-x64-Setup.exe.zip";
            };
            "NW"=@{
                "Wireshark-win64-1.12.6"="https://2.na.dl.wireshark.org/win64/Wireshark-win64-1.12.6.exe";
            }
            #Automated Test
            #"AT"=@{
            #    ""="";
            #    ""="";
            #};
            #"AP"=@{
            #};
        }
    )
    begin{
        #create download folder
        # TODO 既にフォルダが存在すればどうかを確認
        # TODO DL先の空き容量を確認
        # New-Item -ItemType directory -Path "$BaseDir\\$Dirname"
        md -Path $Path | out-null
    }
    process{
        # more beautiful but this code didn't work..
        #$ToolsGroup |
        #    % getEnumerator |
        #        % getEnumerator | % {$key = $($_.key);$value = $($_.value);Write "key=$key,value=$value"}
        Write-Host "Donwload Start.`n"
        foreach($Tools in $ToolsGroup.values){
           foreach($ToolKey in $Tools.keys){
                Write-Host -NoNewLine "Start Downloading $ToolKey"
                Invoke-WebRequest -Uri  $Tools[$ToolKey] -Outfile "$Path\$ToolKey"

            }
        }
        Write-Host "Donwload Done.`n"
    }
    end{
        # todo ダウンロードできているか確認する
    }
}