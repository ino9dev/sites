# �x�[�V�b�N�ȃf�[�^��`�ƃI�y���[�^����
# range operator���g��
(1..10) | % {Write $_}
(1..10) | ForEach {Write $_} # alias1
(1..10) | ForEach-Object {Write $_} # alias2

# �z����g��
(1,2,3) | % {Write $_}

# �A�z�z��i�n�b�V���e�[�u���j���g��
# @{k1=v1,k2=v2) this is defined as [hashtable]
@{"a"=1;"b"=2}.keys | % {Write $_} # list of names
@{"a"=1;"b"=2}.values | % {Write $_} # list of values
 
# Todo ���ڑ��삷��Ƒ���ł��Ȃ����̒����A�I�u�W�F�N�g�̌^���Ⴄ�̂��A
$h = @{"a"=1};$h.Add("b",2);Write $h

# �I�u�W�F�N�g�w���{�v���g�^�C�v
# TODO �O�q�̃����o�[�ϐ��փA�N�Z�X���邽�߂̕��@this.Name�I�ȕ\������肭�ʂ�Ȃ�
#| Add-Member -MemberType ScriptMethod -Name say -Value {"hello"+Get-Member($This.Name)} -PassThru
$MamalClass = {$MyInvocation.MyCommand.ScriptBlock `
    | Add-Member -PassThru -MemberType NoteProperty -Name Name -Value "mamal" `
    | Add-Member -MemberType ScriptMethod -Name hello -Value {"hello"} -PassThru
}
$obj = &$MamalClass
$obj.hello()

# ���t�n
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
# �f�[�^�����n
#
############################################################
function Get-TestDataFile {
    <#
    .DESCRIPTION
    
    ����̃T�C�Y�̃t�@�C���𐶐�����
    
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

        # �N�_�Ƃ���p�X��Test.ps1�̎��s�p�X�i���̖h�~�d�l�j
        $BasePath = (Split-Path ( & { $myInvocation.ScriptName } ) -parent)

        # ���O���w�肳��Ă�����A������N�_�Ɋ܂߂�
        if($Name -ne ""){$BasePath += "/$Name"}
 
        # �f�[�^�̌��ɂȂ�f�[�^�t�@�C���̈ꎞ�ۑ�����쐬����
        $date = (Get-Date).ToString("yyyyMMddhhmmss")
        $tmpfolder = "$BasePath\tmp_$date"
        md $tmpfolder | Out-Null
        
        # �f�[�^�̌��ɂȂ�f�[�^�t�@�C�����쐬����
        ni -Type file -Force -Value $Value "$tmpfolder\$Basefilename"

        # �t�@�C���Z�b�g�쐬
        (1..($Filecount-1)) | % {
            cp "$tmpfolder\$Basefilename" "$tmpfolder\$Basefilename$_" | Out-Null
        }
    }

    #testcase
    # depth=3;filecount=100;
    # 3�K�w(,1����)��100�t�@�C��
    # a1/a2/a3(100)

    #testcase
    # depth�̏�ŁAlevel
    # 3�K�w,2������100�t�@�C��
    # a1/a2/a3 (100)
    # b1/b2/b3 (100)
        
    #testcase
    # 1�K�w,1������100�t�@�C��
    # a (100)

    # 1�K�w,3������100�t�@�C��
    # a (100)
    # b (100)
    # c (100)
    process{

        #todo �w��̒l�Ŗ��߂�i������I�Ɂj
        #todo byte�P�ʂō쐬�ł���悤�ɂ���

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
        # todo ���s�p�X�Ƀt�@�C�����쐬����
        # �摜�f�[�^�����p�̃N���X��ǂݍ���
        Add-Type -AssemblyName System.Drawing

        # �N�_�Ƃ���p�X��Test.ps1�̎��s�p�X�i���̖h�~�d�l�j
        $BasePath = (Split-Path ( & { $myInvocation.ScriptName } ) -parent)
        
        # ���O���w�肳��Ă�����A������N�_�Ɋ܂߂�A���̃t�H���_���쐬���Ă���
        if($Name -ne ""){$BasePath += "\$Name";md $BasePath|Out-null}
    }
    process{

        # �w��̃T�C�Y���摜�f�[�^�𐶐�����
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

        # todo : �w��̐F�̃t�@�C���𐶐�����
        $Colors | % {
            # todo �w��̐F��rectangle�t�@�C���𐶐�����
            # colors-option���w�肷��
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

# urlencode���s��
# �f�t�H���g�ł̓��C�u���������[�h����Ă��Ȃ��̂ł��Ă���
# output : http%3a%2f%2ftest.com%2fgetparam%3d!%23%24%25%26%27(
Add-Type -AssemblyName System.Web
[System.Web.HttpUtility]::UrlEncode("http://test.com/getparam=!#$%&'(")

# urldecode���s��
# output : http://test.com/getparam=!#$%&'(
[System.Web.HttpUtility]::UrlDecode("http%3a%2f%2ftest.com%2fgetparam%3d!%23%24%25%26%27(")

# �������base64��encode����
Write-Host -NoNewline [System.Convert]::ToBase64String([System.Text.Encoding]::UTF8.GetBytes("TEST!#$%&'()"))

# basic�F�ؗp�̕�����𐶐�����
# username, password, basic authentication string�ŏo�͂���
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

# �����_���ȕ�����𐶐�����icharset��^����10�����o�́j
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

# �K���I�ȕ������[�UID,�p�X���[�h�𐶐�����iUSER001-USER010�j
(0..10) | % {Write ($_.ToString("USER000") +","+ $_.ToString("PASS000"))}

# �������[�U���i�p�X���[�h��PASSWORD�j��basic�F�ؕ�����𐶐�����(10���[�U���j 
# �O�q��Get-BasicAuth���g�p����
(0..10) | % {Get-BasicAuth -Username $_.ToString("USER000") -Password $_.ToString("PASS000")}

# 1 dimensional simple random walk
# ������J��Ԃ����ꍇ�ɁA�_������ʒu�ɑ��݂���m���͐��K���z�Ŏ������B
# �ȉ��̗�ł́A100��̃R�C���g�X��10000�񎎍s���Ă���
# depend on function Get-RandomStr

# ((0..99) | % {Get-RandomStr -Charset "01" -Count 1} | % {switch($_){"a"{-1} "b"{+1}}} | Measure -Sum |select -ExpandProperty Sum

(1..100) | % {(1..100) | % {Get-RandomStr -Charset "01" -Count 1} | % {switch($_){"0"{-1} "1"{+1}}} | Measure -Sum | % {echo "$_.Sum"}}

############################################################
#
# �t�@�C���V�X�e���n
#
############################################################
# ����t�H���_�z���̃t�H���_�̂݌���
Get-ChildItem -Path . -Recurse

# ����X�V���t�̃t�@�C���̂݌���

# ����T�C�Y�̃t�@�C���̂݌���

############################################################
#
# �e�X�g�p�c�[���_�E�����[�h
#
############################################################
function DownloadTestTools {
    <#
    .DESCRIPTION �e�X�g�p�̃c�[�����_�E�����[�h���܂��B�e�X�g�^�C�v�ɂ���āA�_�E�����[�h����c�[���Q���w�肵�܂�
    .NOTES ��������function���g�p����ꍇ�ADL��URL�́A�K�؂ɁA�e���悤�ɂ����ύX��������
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
        # TODO ���Ƀt�H���_�����݂���΂ǂ������m�F
        # TODO DL��̋󂫗e�ʂ��m�F
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
        # todo �_�E�����[�h�ł��Ă��邩�m�F����
    }
}