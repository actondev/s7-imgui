#include "gtest/gtest.h"
#include "aod/s7/repl.hpp"
#include <regex>

using namespace aod::s7;

TEST(Repl, Init) {
  ASSERT_EQ(1,1);
}

TEST(Repl, read_simple_exp){
	Repl repl = Repl();
	ASSERT_TRUE(repl.handleInput("(define a (+ 1 2 3))"));
	ASSERT_EQ("6", repl.evalLastForm());
	ASSERT_TRUE(repl.handleInput("(+ a 1)"));
	ASSERT_EQ("7", repl.evalLastForm());
}

TEST(Repl, read_function){
	Repl repl = Repl();
	ASSERT_TRUE(repl.handleInput("(define (inc x) (+ 1 x))"));
	ASSERT_EQ("inc", repl.evalLastForm());
	ASSERT_TRUE(repl.handleInput("(inc 1)"));
	ASSERT_EQ("2", repl.evalLastForm());
}

TEST(Repl, ns_regexp){
    // std::regex ns_regex("^\\(ns [a-zA-Z.-]+\\)");
    std::regex ns_regex = aod::s7::repl::NS_REGEXP;
    ASSERT_FALSE(std::regex_search("ns aod.demo", ns_regex));
    ASSERT_TRUE(std::regex_search("(ns aod.demo)", ns_regex));
    ASSERT_FALSE(std::regex_search(";; (ns aod.demo)", ns_regex));
}


TEST(Repl, read_incomplete_sexp){
	Repl repl = Repl();
	// missing closing paren
	ASSERT_FALSE(repl.handleInput("(write (+ 1 2 3)"));
	// adding the missing paren
	ASSERT_TRUE(repl.handleInput(")"));
	ASSERT_EQ("6", repl.evalLastForm());
}

TEST(Repl, read_2_incomplete_sexps){
	Repl repl = Repl();
	ASSERT_FALSE(repl.handleInput("(+ 1 1")); // missing closing paren
	ASSERT_TRUE(repl.handleInput(")")); // adding the missing paren
	ASSERT_EQ("2", repl.evalLastForm());

	ASSERT_FALSE(repl.handleInput("(+ 1 2")); // missing closing paren
	ASSERT_TRUE(repl.handleInput(")")); // adding the missing paren
	ASSERT_EQ("3", repl.evalLastForm());
}


/**
 * Note: internally Repl wraps the input string around a (begin ..) statement
 * If we wouldn't do that
 * - "(+ 1 1" => cannot be read
 * - ")(+ 1 2" => can be read by completing the previous input that error'd
 *
 * thus inputting a ")" leads to "unexpected close paren"
 *
 * To wrap up, s7_read succeeds in the first complete form.
 * But if we wrap around a begin we get the expected behavior
 */
TEST(Repl, read_2_incomplete_sexps_joined){
	Repl repl = Repl();
	ASSERT_FALSE(repl.handleInput("(+ 1 1")); // note: missing closing paren
	ASSERT_FALSE(repl.handleInput(")(+ 1 2")); // closing missing paren, opening another form

	// closing the parenthesis now returns false (cause we have nothing in memory
	// unexpected close paren
	ASSERT_TRUE(repl.handleInput(")"));
	// last one is 3
	ASSERT_EQ("3", repl.evalLastForm());

	// we should be able to handle correct input though
	ASSERT_TRUE(repl.handleInput("(+ 1 3)"));
	ASSERT_EQ("4", repl.evalLastForm());
}

// aha.. this is .. weird? or expected?
TEST(Repl, evaling_nonexisten_last_form){
	Repl repl = Repl();
	ASSERT_EQ("()", repl.evalLastForm());
}

TEST(Repl, evaling_failed_last_form){
	Repl repl = Repl();
	ASSERT_FALSE(repl.handleInput("(+ 1 1"));
	ASSERT_EQ("()", repl.evalLastForm());
}
