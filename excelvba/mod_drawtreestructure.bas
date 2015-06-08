Attribute VB_Name = "mod_drawtreestructure"

'�g����
'�K�w�\���I�l���܂܂��r�������������Z����I������
'�C�~�f�B�G�C�g�ȂǂŁuDRAW_TREE_STREUCTURE�v�����s����
'�����Feature ���� ����d�l
'��1 �ُ�n�i�Z�����I������Ă��Ȃ��A�����I���Z���̏ꍇ�Ȃǁj�ɑΉ��ł��Ă��Ȃ��̂őΉ�����
'��2 �����l������ɂ������ꍇ�́A����Ƃ݂Ȃ����̂ŁA����ƌ��Ȃ��Ȃ��Ή�������
'��3 ���łɌr����������Ă���ꍇ�͓��ɑΉ��͂��Ȃ��̂ŁA���̌r�����폜����Ή�������
'
'�s�
'��1 ���K�w�A���K�w�ƁA�K�w�������ꏊ�ɓ����l���i�[����Ă���ꍇ�Ɍr���̈�����������������A�K�w�𔻒肷�鏈�����K�v��������Ȃ�
'
'�����b�g
' 
'��1 �K�w�\�����p�[�X���č\�������f�����O���Ă���r���������Ƃ����悤�Ȍ`�ł͂Ȃ��̂ŁA���܂肫�ꂢ�ȃR�[�h�ł͂Ȃ��ėp���͂Ȃ��A�������񎟌��z��I�ɂ������Ă���̂ł���Ɣ�r���đ����r����������i�v����K�v�j
'��2 �X�N���b�`�Ŏ����ŏ����Ă��܂��̂ŁA�s�v�ȃ��C�u������
' 
'���̑�

Public Sub DRAW_TREE_STRUCTURE()
    Dim xlen As Integer
    Dim ylen As Integer
    Dim baseaddr As String
    baseaddr = Selection.Cells(1, 1).Address
    xlen = Selection.Columns.Count
    ylen = Selection.Rows.Count
    
    Dim tx As Integer
    Dim ty As Integer
    
    Dim rs As Integer '�J�n
    Dim re As Integer '�I��
    Dim iv As String  '�Ώۂ̍��ڒl
    Dim cR As Range   '�J�����g�Z��
    
    '���x������
    For tx = 0 To xlen - 1
        For ty = 0 To ylen - 1
            '�e���x�����Ɍr��������
            'Debug.Print range(baseaddr).Offset(tx, ty).Value, tx, ty
            '�l�����菉��̍s�ł͂Ȃ��ꍇ�ɁA�������̋�؂�Ƃ���
            Set cR = Range(Range(baseaddr).Offset(ty, tx).Address)
            '����s�̏ꍇ
            If ty = 0 Then
                If cR.Value <> "" Then
                    iv = cR.Value
                    rs = ty
                End If
            End If
            '����s�̏ꍇ
            If cR.Value <> "" And iv <> cR.Value Then
                iv = cR.Value
                '������commit
                Range(cR.Offset(rs - ty, 0).Address & ":" & cR.Offset(-1, xlen - tx - 1).Address).BorderAround LineStyle:=xlContinuous, Weight:=xlThin
                rs = ty
            End If
            '�ŏI�s�̏ꍇ
            If ty = ylen - 1 Then
                '������commit
                Range(cR.Offset(rs - ty, 0).Address & ":" & cR.Offset(0, xlen - tx - 1).Address).BorderAround LineStyle:=xlContinuous, Weight:=xlThin
            End If
        Next ty
    Next tx
End Sub
