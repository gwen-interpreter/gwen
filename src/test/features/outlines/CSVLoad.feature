Feature: CSV Load

  @Examples("src/test/features/outlines/CSVLoad.csv")
  Scenario Outline: Load from CSV
    Given the website is "${WEBSITE}"
     When I capture the text in the website by regex "(.+?):.*" as the protocol
     Then the protocol should be "https"
