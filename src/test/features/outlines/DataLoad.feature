Feature: Data Load

  @Examples("src/test/features/outlines/Data.csv")
  Scenario Outline: Load from CSV
    Given the website is "${WEBSITE}"
     When I capture the text in the website by regex "(.+?):.*" as the protocol
     Then the protocol should be "https"

  @Examples("src/test/features/outlines/Data.json")
  Scenario Outline: Load from JSON
    Given the website is "${website}"
     When I capture the text in the website by regex "(.+?):.*" as the protocol
     Then the protocol should be "https"
