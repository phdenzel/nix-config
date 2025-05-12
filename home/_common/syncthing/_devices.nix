{
  definitions = {
    "phinix" = {
      id = "Q7GMTVX-CSOMIDN-YPW3QO7-S2FWTJD-RGHDRNO-6Z7NZBH-WHEQ4KB-RO6OXA3";
    };
    "fenrix" = {
      id = "LAX3AZA-OADG4U7-SP3FWXD-YYBIKKS-DHNY3IF-6FIXGYU-VY6W2BI-O773RQD";
    };
    "asahi" = {
      id = "TEK3CC3-2JU5QE2-S55OP5E-PSCFF7K-IMLVPAA-ZBJWE25-4CTNUDB-J7IPIQI";
    };  
  };
  groups = {
    all = ["phinix" "fenrix" "asahi"];
    home = ["phinix" "asahi"];
    work = ["fenrix"];
  };
}
