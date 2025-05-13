library(testthat)
library(tokenizers)
library(textstem)
library(textclean)

# Source the normalize_input function
source("shared.R")
source("config.R") # For log_error

test_that("normalize_input handles various invalid tokens correctly", {
    # Valid English words
    expect_equal(normalize_input("I run running"), c("i", "run", "run"), info = "Valid words")
    
    # Hashtags
    expect_equal(normalize_input("#lebronjames play"), c("play"), info = "Hashtag removed")
    expect_equal(normalize_input("##tag #nba"), NULL, info = "Only hashtags")
    expect_equal(normalize_input("#urmc game"), c("game"), info = "Hashtag urmc removed")
    
    # Numbers
    expect_equal(normalize_input("123 456"), NULL, info = "Only numbers")
    expect_equal(normalize_input("run 123 play"), c("run", "play"), info = "Numbers filtered")
    
    # Alphanumeric
    expect_equal(normalize_input("nhl12 game"), c("game"), info = "Alphanumeric filtered")
    expect_equal(normalize_input("abc123 def456"), NULL, info = "Only alphanumeric")
    
    # Special characters
    expect_equal(normalize_input("@user $price"), NULL, info = "Only special chars")
    expect_equal(normalize_input("talk @user play"), c("talk", "play"), info = "Special chars filtered")
    
    # Symbols
    expect_equal(normalize_input("!@#$%^&*()_+=[]{}|\\:;\"'<>,.?/~`"), NULL, info = "Only symbols")
    expect_equal(normalize_input("run !@# play"), c("run", "play"), info = "Symbols filtered")
    
    # URLs
    expect_equal(normalize_input("http://example.com"), NULL, info = "Only URL")
    expect_equal(normalize_input("visit http://example.com now"), c("visit", "now"), info = "URL filtered")
    
    # Emojis
    expect_equal(normalize_input("smile 😊"), c("smile"), info = "Emoji filtered")
    expect_equal(normalize_input("😊👍"), NULL, info = "Only emojis")
    
    # Malformed text
    expect_equal(normalize_input("a_b ##tag"), NULL, info = "Only malformed")
    expect_equal(normalize_input("run a_b play"), c("run", "play"), info = "Malformed filtered")
    
    # Foreign characters
    expect_equal(normalize_input("café über"), c("cafe", "uber"), info = "Foreign chars normalized")
    expect_equal(normalize_input("привет 日本語"), NULL, info = "Only Cyrillic/CJK")
    expect_equal(normalize_input("talk café play"), c("talk", "cafe","play"), info = "Foreign chars normalized")
    
    # Mixed cases
    expect_equal(normalize_input("i run #lebronjames café 123 %^&* 日本語"), c("i", "run", "cafe"), info = "Mixed invalid filtered")
    expect_equal(normalize_input("talk #urmc nhl12 @user 😊"), c("talk"), info = "Mixed invalid with urmc")
})

test_that("normalize_input handles edge cases and errors", {
    # Empty string
    expect_equal(normalize_input(""), NULL, info = "Empty string")
    
    # All invalid
    expect_equal(normalize_input("#lebronjames 123 @user 😊 café 日本語 !@#"), c("cafe"), info = "All invalid")
    
    # Long mixed string
    long_text <- paste("talk play #lebronjames 123 nhl12 @user http://example.com 😊 café",
                       "привет 日本語 !@#$%^&*() run", sep = " ")
    expect_equal(normalize_input(long_text), c("talk", "play", "cafe", "run"), info = "Long mixed")
    
    # Very long invalid
    long_invalid <- paste(rep("#lebronjames 123 @user 😊", 100), collapse = " ")
    expect_equal(normalize_input(long_invalid), NULL, info = "Long invalid")
    
    # Single valid word
    expect_equal(normalize_input("i"), "i", info = "Single i")
    
    # Contractions
    expect_equal(normalize_input("can't run"), c("can not", "run"), info = "Contraction")
    
    # Malformed input
    #expect_warning(normalize_input(NA), NULL, info = "NA input")
    expect_equal(normalize_input(NA), NULL, info = "Invalid or empty input")
})