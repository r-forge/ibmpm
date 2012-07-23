<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->
<?php

$domain = preg_replace('/[^\.]*\.(.*)$/','\1',$_SERVER['HTTP_HOST']);
$group_name = preg_replace('/([^\.]*)\..*$/','\1',$_SERVER['HTTP_HOST']);
$themeroot = 'http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="R.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<h1>ibmpm - R package for individual-based metapopulation simulation</h1>

<!-- end of project description -->

<p><a href="ibmpm-manual.pdf">Manual in PDF</a></p>

<p>Install development version from R-forge: <br />
<p><tt style="font-weight:bold; color: #005f8c;">&gt; install.packages("ibmpm", repos="http://R-Forge.R-project.org")</tt></p>


<b><a href="http://r-forge.r-project.org/R/?group_id=1447">Download</a></b></p>

<p>The <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>project summary page</strong></a>. </p>

<hr />

<p>Alternatively, download packages from here:</p>

<?php

$pkg_types = array('tar.gz' => 'Package source', 'zip' => 'Windows zip');

$pattern = "/^ibmpm_(.+)\\.(tar\\.gz|zip)+$/";
$d = dir(".");
while (false !== ($entry = $d->read())) {
	preg_match($pattern, $entry, $matches);
	if (empty($matches)) continue;

	echo "<div><b>${pkg_types[$matches[2]]}</b>: <a href=\"$entry\">version $matches[1]</a></div>";
}
$d->close();
?> 

</body>
</html>
