package ca.innovativemedicine

package object vcf {
  type VcfRow = (Variant, List[Metadata.Format], List[List[List[VcfValue]]])
}