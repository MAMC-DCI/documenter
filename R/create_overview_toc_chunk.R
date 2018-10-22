#' Create a table of contents chunk for the overview section.
#'
#' @param page_ref The page_ref ID.
#'
#' @examples
#' create_toc_chunk(filename, page_ref)
create_overview_toc_chunk <- function(
  page_ref
){
  # Create the toc_chunk.
  toc_chunk <- paste0(
    '<w:hyperlink w:anchor="_Toc527442460" w:history="1"><w:r w:rsidRPr=',
    '"006636B7"><w:rPr><w:rStyle w:val="Hyperlink"/><w:noProof/></w:rPr><w:t>',
    "Overview",
    '</w:t></w:r><w:r><w:rPr><w:noProof/><w:webHidden/></w:rPr><w:tab/>',
    '</w:r><w:r><w:rPr><w:noProof/><w:webHidden/></w:rPr><w:fldChar ',
    'w:fldCharType="begin"/></w:r><w:r><w:rPr><w:noProof/><w:webHidden/>',
    '</w:rPr><w:instrText xml:space="preserve"> PAGEREF ',
    page_ref,
    '\\h </w:instrText></w:r><w:r><w:rPr><w:noProof/><w:webHidden/></w:rPr>',
    '</w:r><w:r><w:rPr><w:noProof/><w:webHidden/></w:rPr><w:fldChar ',
    'w:fldCharType="separate"/></w:r><w:r><w:rPr><w:noProof/><w:webHidden/>',
    '</w:rPr><w:t>4</w:t></w:r><w:r><w:rPr><w:noProof/><w:webHidden/>',
    '</w:rPr><w:fldChar w:fldCharType="end"/></w:r></w:hyperlink></w:p>',
    '<w:p w:rsidR="00E152FA" w:rsidRDefault="00E152FA"><w:pPr><w:pStyle' ,
    'w:val="TOC1"/><w:tabs><w:tab w:val="right" w:leader="dot" w:pos="9350"/>',
    '</w:tabs><w:rPr><w:rFonts w:eastAsiaTheme="minorEastAsia"/><w:noProof/>',
    '</w:rPr></w:pPr>'
  )

  # Return the annotations.
  return(toc_chunk)
}