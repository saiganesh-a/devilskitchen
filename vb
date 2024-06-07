Imports FNS.FrontEnd.Workflow.Server

Friend Class CFNSDecision : Implements FlowDistributorTypes.IWorkflowComponentInterface

    'Private oLogger As New FlowDistributorTypes.CFNSEventLogger("CFNSDecision")

    Public Function processDocument(ByVal ComponentDocuments As FlowDistributorTypes.CFNSComponentDocuments) As FlowDistributorTypes.CFNSComponentDocuments Implements FlowDistributorTypes.IWorkflowComponentInterface.processDocument

        Dim nCurrentInDoc As Integer
        Dim xmlCurrentInDoc As FlowDistributorTypes.CFNSComponentDocument
        Dim xmlCurrentOutDoc As FlowDistributorTypes.CFNSComponentDocument
        Dim xmlReturnDocuments As FlowDistributorTypes.CFNSComponentDocuments

        Try
            xmlReturnDocuments = ComponentDocuments.createReturnDocuments
            For nCurrentInDoc = 0 To ComponentDocuments.ComponentDocumentCount - 1
                xmlCurrentInDoc = ComponentDocuments.ComponentDocument(nCurrentInDoc)
                xmlCurrentOutDoc = xmlReturnDocuments.ComponentDocument(nCurrentInDoc)

                Select Case xmlCurrentInDoc.methodName.ToUpper

                    Case "IF"
                        xmlCurrentOutDoc.methodResult = DoIf(xmlCurrentInDoc)

                    Case "SELECT"
                        xmlCurrentOutDoc.methodResult = DoSelect(xmlCurrentInDoc)

                    Case Else
                        Throw New Exception("Invalid method name: " + xmlCurrentInDoc.methodName)

                End Select
            Next

            Return xmlReturnDocuments

        Catch e As Exception
            'oLogger.WriteEntry(e.Message, EventLogEntryType.Error)
            Throw e
        End Try



    End Function


    'If Example:
    '<method result="True" name="If">
    '	<decisionCriteria>
    '		<expression1 type="xpath">//screenData/OutputType/text()</expression1>
    '		<expression2 type="literal">08</expression2> <!--ok warning-->
    '		<compare>=</compare>
    '	</decisionCriteria>
    '</method>
    Private Function DoIf(ByVal xmlCurrentInDoc As FlowDistributorTypes.CFNSComponentDocument) As String
        Dim sExpression1Result As String
        Dim sExpression2Result As String
        Dim sCompare As String
        'Dim nodeCompare As Xml.XmlNode
        Dim bNumericCompare As Boolean

        sExpression1Result = xmlCurrentInDoc.getMethodParameterText("expression1", True)
        sExpression2Result = xmlCurrentInDoc.getMethodParameterText("expression2", True)
        'nodeCompare = xmlCurrentInDoc.getMethodParameterNode("compare", True)
        'bNumericCompare = nodeCompare.Attributes.GetNamedItem("numericCompare").InnerText()
        sCompare = xmlCurrentInDoc.getMethodParameterText("compare", True)

        If IsNumeric(sExpression1Result) And IsNumeric(sExpression2Result) Then
            bNumericCompare = True
        End If
        'Start of ir 15090026 Bhoomil
        'Select Case sCompare

        
        Select Case sCompare.ToUpper()
            'End of ir 15090026 Bhoomil
            Case "=", "=="
                If bNumericCompare Then
                    Return CType(Val(sExpression1Result) = Val(sExpression2Result), String)
                Else
                    Return CType(sExpression1Result = sExpression2Result, String)
                End If

            Case "<"
                If bNumericCompare Then
                    Return CType(Val(sExpression1Result) < Val(sExpression2Result), String)
                Else
                    Return CType(sExpression1Result < sExpression2Result, String)
                End If

            Case ">"
                If bNumericCompare Then
                    Return CType(Val(sExpression1Result) > Val(sExpression2Result), String)
                Else
                    Return CType(sExpression1Result > sExpression2Result, String)
                End If

            Case ">="
                If bNumericCompare Then
                    Return CType(Val(sExpression1Result) >= Val(sExpression2Result), String)
                Else
                    Return CType(sExpression1Result >= sExpression2Result, String)
                End If

            Case "<="
                If bNumericCompare Then
                    Return CType(Val(sExpression1Result) <= Val(sExpression2Result), String)
                Else
                    Return CType(sExpression1Result <= sExpression2Result, String)
                End If

            Case "!=", "<>"
                If bNumericCompare Then
                    Return CType(Val(sExpression1Result) <> Val(sExpression2Result), String)
                Else
                    Return CType(sExpression1Result <> sExpression2Result, String)
                End If
                'Start of IR 15090026 Bhoomil
            Case "CONTAINS", "LIKE"
                Dim Ex1 As String = ""
                Ex1 = sExpression1Result.ToUpper()
                Dim Ex2 As String = ""
                Ex2 = sExpression2Result.ToUpper()
                If Ex1.IndexOf(Ex2) <> -1 Then
                    Return True
                Else
                    Return False
                End If
                'End of IR 15090026 Bhoomil
            Case Else
                Throw New Exception("Invalid relational operator """ + sCompare + """")

        End Select
    End Function

    'Case Example
    '<method result="Multiple" name="case">
    '	<select>
    '		<expression type="xpath">//screenData/OutputType/text()</expression>
    '		<case outcomeStatus="PassBookUpdate">00</case>
    '		<case outcomeStatus="ErrorMessage">01</case>
    '		<case outcomeStatus="ChequePrint">02</case>
    '		<case outcomeStatus="MainScreen">03</case>
    '		<case outcomeStatus="OkMessage">04</case>
    '		<default outcomeStatus="ErrorStatusbar2"></default>
    '	</select>
    '</method>
    Private Function DoSelect(ByVal xmlCurrentInDoc As FlowDistributorTypes.CFNSComponentDocument) As String


        Dim sExpressionResult = xmlCurrentInDoc.getMethodParameterText("expression", True)
        Dim nodeCase As Xml.XmlNode
        Dim bMatched As Boolean

        ' check each case
        For Each nodeCase In xmlCurrentInDoc.method.SelectNodes("case")
            

            If IsNumeric(sExpressionResult) And IsNumeric(nodeCase.InnerText) Then
                bMatched = Val(sExpressionResult) = Val(nodeCase.InnerText)
            Else
                bMatched = sExpressionResult = nodeCase.InnerText
            End If

            If bMatched Then

                Return nodeCase.Attributes.GetNamedItem("outcomeStatus").InnerText

            End If
        Next

        ' no match found. do default
        Try

            Return xmlCurrentInDoc.method.SelectSingleNode("default").Attributes.GetNamedItem("outcomeStatus").InnerText
        Catch
            Throw New Exception("Error evaluating case node. No default case specified.")
        End Try
    End Function




End Class
