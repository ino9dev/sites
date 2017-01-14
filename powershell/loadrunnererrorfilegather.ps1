############################################################
#
# データ生成系
#
############################################################
function GetLoadRunnerGenerao{
    <#
    .DESCRIPTION
    
    LoadRunner関連の処理を自動化する
    
    .EXAMPLE

    LoadRunnerUtil -Cmd Error -ErrorLogPath
    
    Get-TestDataFile -Filecount 10
    Get-TestDataFile -Depth 5
    Get-TestDataFile -Depth 1 -Level 2 -Path "testcase2"
    Get-TestDataFile -Depth 2 -Level 1 -Path "testcase3"

    #>
    param(
        #
        [Parameter(ValueFromPipeline=$true)]
        [int32]$Depth=1,
        [String]$Name=""
    )
    begin{
    
        #path check
        #input value check

    }
    #main process
    process{

        
    
    }
    end{}
}
