package species.sparql

trait Extras {
  protected def up(str: String, sep2: String = ";")(implicit no_prefix: Boolean): String = if(!no_prefix || str.startsWith("http")) str else if(str.contains(sep2)){
    str.split(sep2).map(s=>up(s, sep2)(no_prefix)).mkString(";")
  } else if(str.contains(":"))  if(str.startsWith("ncbi:bioproject/")) str.replace("ncbi:bioproject/","") else str.substring(str.indexOf(":")+1) else str


  protected def delimiter(str: String) = if(str.contains(",")) "," else ";"

}
