<% for (varn in names(mtcars[,1:2))   { %><%=
  var=mtcars[,varn]
pandoc.p.return("")
pandoc.header.return(paste("Results for:",varn,""),3)
pandoc.p.return("")
fac=(100*log(length(unique(var))))#calculate some factor to ensure somewhat wider graphs for more bins

%><% evals.option("width",50+fac) %><%= #have to break out of the BRCODES to change the height options for the next chunk
  qplot(var,geom="bar")+xlab(varn)%>
  </br>Anything you type here will be inside the same paragraph as the figure and so works like a pseudocaption<%
#   coord_flip()
%><% } %>