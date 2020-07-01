#include "gtest/gtest.h"
#include "s7.h"
#include "aod/s7.hpp"

TEST(FFI, Dummy) {
	ASSERT_EQ(1, 1);
	s7_scheme *sc = s7_init();
	s7_eval_c_string(sc, "(define b #t)");

	s7_pointer res1_sc = s7_eval_c_string(sc, "b");
	bool res1_b = s7_boolean(sc, res1_sc);
	ASSERT_EQ(true, res1_b);

	s7_eval_c_string(sc, "(set! b #f)");
	s7_pointer res2_sc = s7_eval_c_string(sc, "b");
	bool res2_b = s7_boolean(sc, res2_sc);
	ASSERT_EQ(false, res2_b);
}

// so.. how would we make a reference to a bool?
// from ffitest.c

typedef struct {
	size_t size;
	double *data;
} g_block;

// no idea why this is 0
static s7_int g_block_type = 0;
static s7_pointer g_block_methods;
#define g_make_block_help "(make-block size) returns a new block of the given size"

static s7_pointer g_make_block(s7_scheme *sc, s7_pointer args) {
	g_block *g;
	s7_pointer new_g;
	g = (g_block*) calloc(1, sizeof(g_block));
	g->size = (size_t) s7_integer(s7_car(args));
	g->data = (double*) calloc(g->size, sizeof(double));
	new_g = s7_make_c_object(sc, g_block_type, (void*) g);
//	s7_c_type_na

	// hey.. wtf is this..?
	// this was causing to crash
//	s7_c_object_set_let(sc, new_g, g_block_methods);
//	s7_openlet(sc, new_g);


	return (new_g);
}

static s7_pointer g_to_block(s7_scheme *sc, s7_pointer args) {
#define g_block_help "(block ...) returns a block c_object with the arguments as its contents."
	s7_pointer p, b;
	size_t i, len;
	g_block *gb;
	len = s7_list_length(sc, args);
	b = g_make_block(sc, s7_cons(sc, s7_make_integer(sc, len), s7_nil(sc)));
	gb = (g_block*) s7_c_object_value(b);
	for (i = 0, p = args; i < len; i++, p = s7_cdr(p))
		gb->data[i] = s7_number_to_real(sc, s7_car(p));
	return (b);
}

static s7_pointer g_block_to_string(s7_scheme *sc, s7_pointer args) {
	return (s7_make_string(sc, "<block>"));
}

static void g_block_free(void *value) {
	fprintf(stderr, "g_block_free called");
	g_block *g = (g_block*) value;
	free(g->data);
	free(g);
}

static bool g_blocks_are_eql(void *val1, void *val2) {
	s7_int i, len;
	g_block *b1 = (g_block*) val1;
	g_block *b2 = (g_block*) val2;
	if (val1 == val2)
		return (true);
	len = b1->size;
	
	if (len != (s7_int)b2->size)
		return (false);
	for (i = 0; i < len; i++)
		if (b1->data[i] != b2->data[i])
			return (false);
	return (true);
}

static s7_pointer g_blocks_are_equal(s7_scheme *sc, s7_pointer args) {
	return (s7_make_boolean(sc,
			g_blocks_are_eql((void*) s7_c_object_value(s7_car(args)),
					(void*) s7_c_object_value(s7_cadr(args)))));
}

static s7_pointer g_blocks_are_equivalent(s7_scheme *sc, s7_pointer args) {
	s7_pointer v1, v2, arg1, arg2;
	g_block *g1, *g2;
	bool result;
	uint32_t gc1, gc2;
	size_t len;
	arg1 = s7_car(args);
	arg2 = s7_cadr(args);
	if (!s7_is_c_object(arg2))
		return (s7_f(sc));
	if (arg1 == arg2)
		return (s7_make_boolean(sc, true));
	if (s7_is_let(arg1)) /* (block-let (block)) */
		return (s7_make_boolean(sc, false)); /* checked == above */
	g1 = (g_block*) s7_c_object_value(arg1);
	if (s7_c_object_type(arg2) != g_block_type)
		return (s7_make_boolean(sc, false));
	g2 = (g_block*) s7_c_object_value(arg2);
	len = g1->size;
	if (len != g2->size)
		return (s7_make_boolean(sc, false));
	v1 = s7_make_float_vector_wrapper(sc, len, g1->data, 1, NULL, false);
	gc1 = s7_gc_protect(sc, v1);
	v2 = s7_make_float_vector_wrapper(sc, len, g2->data, 1, NULL, false);
	gc2 = s7_gc_protect(sc, v2);
	result = s7_is_equivalent(sc, v1, v2);
	s7_gc_unprotect_at(sc, gc1);
	s7_gc_unprotect_at(sc, gc2);
	return (s7_make_boolean(sc, result));
}

static s7_pointer g_block_ref(s7_scheme *sc, s7_pointer args) {
	g_block *g;
	int index;
	g = (g_block*) s7_c_object_value(s7_car(args));
	index = s7_integer(s7_cadr(args));
	printf("index %d\n", index);
	if (index < (s7_int)g->size)
		return (s7_make_real(sc, g->data[index]));
	return (s7_out_of_range_error(sc, "block-ref", 2, s7_cadr(args),
			"should be less than block length"));
}

static s7_pointer g_block_set(s7_scheme *sc, s7_pointer args) {
	g_block *g;
	s7_int index;
	// 3 args: (block-set! block index value)
	if (s7_list_length(sc, args) != 3)
		return (s7_wrong_number_of_args_error(sc,
				"block-set! takes 3 arguments: ~S", args));
	g = (g_block*) s7_c_object_value(s7_car(args));
	index = s7_integer(s7_cadr(args));
	if ((index >= 0) && (index < (s7_int)g->size)) {
		g->data[index] = s7_number_to_real(sc, s7_caddr(args));
		return (s7_caddr(args));
	}
	return (s7_out_of_range_error(sc, "block-set", 2, s7_cadr(args),
			"should be less than block length"));
}

static s7_pointer g_block_length(s7_scheme *sc, s7_pointer args) {
	g_block *g = (g_block*) s7_c_object_value(s7_car(args));
	return (s7_make_integer(sc, g->size));
}

static void g_block_mark(void *val) {
	/* nothing to mark */
}

static s7_pointer g_block_copy(s7_scheme *sc, s7_pointer args) {
	s7_pointer obj, new_g;
	g_block *g, *g1;
	obj = s7_car(args);
	g = (g_block*) s7_c_object_value(obj);
	new_g = g_make_block(sc,
			s7_cons(sc, s7_make_integer(sc, g->size), s7_nil(sc)));
	g1 = (g_block*) s7_c_object_value(new_g);
	memcpy((void*) (g1->data), (void*) (g->data), g->size * sizeof(double));
	return (new_g);
}

static s7_pointer g_block_reverse(s7_scheme *sc, s7_pointer args) {
	size_t i, j;
	s7_pointer obj, new_g;
	g_block *g, *g1;
	obj = s7_car(args);
	g = (g_block*) s7_c_object_value(obj);
	new_g = g_make_block(sc,
			s7_cons(sc, s7_make_integer(sc, g->size), s7_nil(sc)));
	g1 = (g_block*) s7_c_object_value(new_g);
	for (i = 0, j = g->size - 1; i < g->size; i++, j--)
		g1->data[i] = g->data[j];
	return (new_g);
}

static s7_pointer g_block_fill(s7_scheme *sc, s7_pointer args) {
	s7_pointer obj;
	size_t i;
	double fill_val;
	g_block *g;
	obj = s7_car(args);
	g = (g_block*) s7_c_object_value(obj);
	fill_val = s7_number_to_real(sc, s7_cadr(args));
	for (i = 0; i < g->size; i++)
		g->data[i] = fill_val;
	return (obj);
}

TEST(FFI, g_block) {
	s7_scheme *sc = s7_init();

	g_block_type = s7_make_c_type(sc, "<block>");
	s7_c_type_set_free(sc, g_block_type, g_block_free);
	s7_c_type_set_equal(sc, g_block_type, g_blocks_are_eql);
	s7_c_type_set_is_equal(sc, g_block_type, g_blocks_are_equal);
	s7_c_type_set_is_equivalent(sc, g_block_type, g_blocks_are_equivalent);
	s7_c_type_set_mark(sc, g_block_type, g_block_mark);
	s7_c_type_set_ref(sc, g_block_type, g_block_ref);
	s7_c_type_set_set(sc, g_block_type, g_block_set);
	s7_c_type_set_length(sc, g_block_type, g_block_length);
	s7_c_type_set_copy(sc, g_block_type, g_block_copy);
	s7_c_type_set_reverse(sc, g_block_type, g_block_reverse);
	s7_c_type_set_fill(sc, g_block_type, g_block_fill);
	s7_c_type_set_to_string(sc, g_block_type, g_block_to_string);


	// usage:
	s7_define_function(sc, "make-block", g_make_block, 1, 0, false,
	g_make_block_help);
	s7_define_function(sc, "block", g_to_block, 0, 0, true, g_block_help);

	return;

	// huh.. without this it crashes!
	g_block_methods = s7_eval_c_string(sc,
			"(inlet (cons 'vector? (lambda (p) #t)))");
	// s7_gc_protect(sc, g_block_methods);




	//

	// g_block *g;
	s7_pointer gp;

	gp = g_make_block(sc, s7_list(sc, 1, s7_make_integer(sc, 32)));
	// s7_int gc_loc = s7_gc_protect(sc, gp);
	char *s1;
	if (!s7_is_c_object(gp)) {
		fprintf(stderr, "%d: g_block %s is not a c_object?\n", __LINE__, s1 =
				s7_object_to_c_string(sc, gp));
		free(s1);
	}

	s7_eval_c_string(sc, "(define b (block 1 2 3))");
	ASSERT_EQ(1, s7_real(s7_eval_c_string(sc, "(b 0)")));
	ASSERT_EQ(2, s7_real(s7_eval_c_string(sc, "(b 1)")));
	ASSERT_EQ(3, s7_real(s7_eval_c_string(sc, "(b 2)")));
}

static s7_pointer g_block_free_gc(s7_scheme *sc, s7_pointer obj) {
	fprintf(stderr, "g_block_free called");
	g_block *g = (g_block*) s7_c_object_value(obj);
	free(g->data);
	free(g);
	return NULL;
}
static s7_pointer g_block_mark_gc(s7_scheme *sc, s7_pointer obj) {
	/* nothing to mark
	 * in our structure we don't have any s7_pointer data.
	 * if we did, we'd need to mark it to not get collected
	 * */
	return NULL;
}
/*
 * From s7.h
 *
 * These functions create a new Scheme object type.  There is a simple example in s7.html.
 *
 * s7_make_c_type creates a new C-based type for Scheme.  It returns an s7_int "tag" used to indentify this type elsewhere.
 *   The functions associated with this type are set via s7_c_type_set*:
 *
 *   free:          the function called when an object of this type is about to be garbage collected
 *   mark:          called during the GC mark pass -- you should call s7_mark
 *                  on any embedded s7_pointer associated with the object (including its "let") to protect if from the GC.
 *   gc_mark and gc_free are new forms of mark and free, taking the c_object s7_pointer rather than its void* value
 *   equal:         compare two objects of this type; (equal? obj1 obj2) -- this is the old form
 *   is_equal:      compare objects as in equal? -- this is the new form of equal?
 *   is_equivalent: compare objects as in equivalent?
 *   ref:           a function that is called whenever an object of this type
 *                  occurs in the function position (at the car of a list; the rest of the list
 *                  is passed to the ref function as the arguments: (obj ...))
 *   set:           a function that is called whenever an object of this type occurs as
 *                  the target of a generalized set! (set! (obj ...) val)
 *   length:        the function called when the object is asked what its length is.
 *   copy:          the function called when a copy of the object is needed.
 *   fill:          the function called to fill the object with some value.
 *   reverse:       similarly...
 *   to_string:     object->string for an object of this type
 *   getter/setter: these help the optimizer handle applicable c-objects (see s7test.scm for an example)
 *
 * s7_is_c_object returns true if 'p' is a c_object
 * s7_c_object_type returns the c_object's type (the s7_int passed to s7_make_c_object)
 * s7_c_object_value returns the value bound to that c_object (the void *value of s7_make_c_object)
 * s7_make_c_object creates a new Scheme entity of the given type with the given (uninterpreted) value
 * s7_mark marks any Scheme c_object as in-use (use this in the mark function to mark
 *    any embedded s7_pointer variables).
 */
TEST(FFI, g_block2) {
	s7_scheme *sc = s7_init();

	int type = s7_make_c_type(sc, "<block>");
	s7_c_type_set_gc_free(sc, type, g_block_free_gc);
	s7_c_type_set_gc_mark(sc, type, g_block_mark_gc);
	s7_c_type_set_ref(sc, type, g_block_ref);
	s7_c_type_set_set(sc, type, g_block_set);
//	s7_c_type_set_length(sc, g_block_type, g_block_length);
//	s7_c_type_set_copy(sc, g_block_type, g_block_copy);
//	s7_c_type_set_fill(sc, g_block_type, g_block_fill);
//	s7_c_type_set_to_string(sc, g_block_type, g_block_to_string);

	s7_define_function(sc, "block", g_to_block, 0, 0, true, g_block_help);


	// g_block *g;
	// s7_pointer gp;

	s7_eval_c_string(sc, "(define b (block 1 2 3))");
	ASSERT_EQ(1, s7_real(s7_eval_c_string(sc, "(b 0)")));
	ASSERT_EQ(2, s7_real(s7_eval_c_string(sc, "(b 1)")));
	ASSERT_EQ(3, s7_real(s7_eval_c_string(sc, "(b 2)")));

	// setting
	s7_eval_c_string(sc, "(set! (b 1) 11)");
	ASSERT_EQ(11, s7_real(s7_eval_c_string(sc, "(b 1)")));

	aod::s7::set_print_stderr(sc);
//	s7_pointer res = s7_eval_c_string(sc, "(display b)");
	// will print something like <<block> 0x55e76935f7b8>
}


TEST(FFI, g_block_no_ref) {
	s7_scheme *sc = s7_init();

	g_block_type = s7_make_c_type(sc, "<block>");
	s7_c_type_set_gc_free(sc, g_block_type, g_block_free_gc);
	s7_c_type_set_gc_mark(sc, g_block_type, g_block_mark_gc);
	s7_c_type_set_ref(sc, g_block_type, g_block_ref);
	s7_c_type_set_set(sc, g_block_type, g_block_set);
//	s7_c_type_set_length(sc, g_block_type, g_block_length);
//	s7_c_type_set_copy(sc, g_block_type, g_block_copy);
//	s7_c_type_set_fill(sc, g_block_type, g_block_fill);
//	s7_c_type_set_to_string(sc, g_block_type, g_block_to_string);

	s7_define_function(sc, "block", g_to_block, 0, 0, true, g_block_help);


	// g_block *g;
	// s7_pointer gp;

	aod::s7::set_print_stderr(sc);
	s7_eval_c_string(sc, "(define b (block 1 2 3))");
	ASSERT_EQ(1, s7_real(s7_eval_c_string(sc, "(b 0)")));



//	s7_pointer res = s7_eval_c_string(sc, "(display (b))");
}
