#pragma once

/*
 * Headers
*/

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#define CL_USE_DEPRECATED_OPENCL_1_2_APIS
#ifdef __APPLE__
#define CL_SILENCE_DEPRECATION
#include <OpenCL/cl.h>
#else
#include <CL/cl.h>
#endif


/*
 * Initialisation
*/

int futhark_get_num_sizes(void);
const char *futhark_get_size_name(int);
const char *futhark_get_size_class(int);
struct futhark_context_config ;
struct futhark_context_config *futhark_context_config_new(void);
void futhark_context_config_free(struct futhark_context_config *cfg);
void futhark_context_config_add_build_option(struct futhark_context_config *cfg,
                                             const char *opt);
void futhark_context_config_set_debugging(struct futhark_context_config *cfg,
                                          int flag);
void futhark_context_config_set_profiling(struct futhark_context_config *cfg,
                                          int flag);
void futhark_context_config_set_logging(struct futhark_context_config *cfg,
                                        int flag);
void futhark_context_config_set_device(struct futhark_context_config *cfg, const
                                       char *s);
void futhark_context_config_set_platform(struct futhark_context_config *cfg,
                                         const char *s);
void
futhark_context_config_select_device_interactively(struct futhark_context_config *cfg);
void futhark_context_config_dump_program_to(struct futhark_context_config *cfg,
                                            const char *path);
void
futhark_context_config_load_program_from(struct futhark_context_config *cfg,
                                         const char *path);
void futhark_context_config_dump_binary_to(struct futhark_context_config *cfg,
                                           const char *path);
void futhark_context_config_load_binary_from(struct futhark_context_config *cfg,
                                             const char *path);
void
futhark_context_config_set_default_group_size(struct futhark_context_config *cfg,
                                              int size);
void
futhark_context_config_set_default_num_groups(struct futhark_context_config *cfg,
                                              int num);
void
futhark_context_config_set_default_tile_size(struct futhark_context_config *cfg,
                                             int num);
void
futhark_context_config_set_default_threshold(struct futhark_context_config *cfg,
                                             int num);
int futhark_context_config_set_size(struct futhark_context_config *cfg, const
                                    char *size_name, size_t size_value);
struct futhark_context ;
struct futhark_context *futhark_context_new(struct futhark_context_config *cfg);
struct futhark_context
*futhark_context_new_with_command_queue(struct futhark_context_config *cfg,
                                        cl_command_queue queue);
void futhark_context_free(struct futhark_context *ctx);
int futhark_context_sync(struct futhark_context *ctx);
char *futhark_context_get_error(struct futhark_context *ctx);
void futhark_context_pause_profiling(struct futhark_context *ctx);
void futhark_context_unpause_profiling(struct futhark_context *ctx);
int futhark_context_clear_caches(struct futhark_context *ctx);
cl_command_queue futhark_context_get_command_queue(struct futhark_context *ctx);

/*
 * Arrays
*/

struct futhark_f32_1d ;
struct futhark_f32_1d *futhark_new_f32_1d(struct futhark_context *ctx,
                                          float *data, int64_t dim0);
struct futhark_f32_1d *futhark_new_raw_f32_1d(struct futhark_context *ctx,
                                              cl_mem data, int offset,
                                              int64_t dim0);
int futhark_free_f32_1d(struct futhark_context *ctx,
                        struct futhark_f32_1d *arr);
int futhark_values_f32_1d(struct futhark_context *ctx,
                          struct futhark_f32_1d *arr, float *data);
cl_mem futhark_values_raw_f32_1d(struct futhark_context *ctx,
                                 struct futhark_f32_1d *arr);
int64_t *futhark_shape_f32_1d(struct futhark_context *ctx,
                              struct futhark_f32_1d *arr);
struct futhark_f32_2d ;
struct futhark_f32_2d *futhark_new_f32_2d(struct futhark_context *ctx,
                                          float *data, int64_t dim0,
                                          int64_t dim1);
struct futhark_f32_2d *futhark_new_raw_f32_2d(struct futhark_context *ctx,
                                              cl_mem data, int offset,
                                              int64_t dim0, int64_t dim1);
int futhark_free_f32_2d(struct futhark_context *ctx,
                        struct futhark_f32_2d *arr);
int futhark_values_f32_2d(struct futhark_context *ctx,
                          struct futhark_f32_2d *arr, float *data);
cl_mem futhark_values_raw_f32_2d(struct futhark_context *ctx,
                                 struct futhark_f32_2d *arr);
int64_t *futhark_shape_f32_2d(struct futhark_context *ctx,
                              struct futhark_f32_2d *arr);
struct futhark_f32_3d ;
struct futhark_f32_3d *futhark_new_f32_3d(struct futhark_context *ctx,
                                          float *data, int64_t dim0,
                                          int64_t dim1, int64_t dim2);
struct futhark_f32_3d *futhark_new_raw_f32_3d(struct futhark_context *ctx,
                                              cl_mem data, int offset,
                                              int64_t dim0, int64_t dim1,
                                              int64_t dim2);
int futhark_free_f32_3d(struct futhark_context *ctx,
                        struct futhark_f32_3d *arr);
int futhark_values_f32_3d(struct futhark_context *ctx,
                          struct futhark_f32_3d *arr, float *data);
cl_mem futhark_values_raw_f32_3d(struct futhark_context *ctx,
                                 struct futhark_f32_3d *arr);
int64_t *futhark_shape_f32_3d(struct futhark_context *ctx,
                              struct futhark_f32_3d *arr);
struct futhark_i32_1d ;
struct futhark_i32_1d *futhark_new_i32_1d(struct futhark_context *ctx,
                                          int32_t *data, int64_t dim0);
struct futhark_i32_1d *futhark_new_raw_i32_1d(struct futhark_context *ctx,
                                              cl_mem data, int offset,
                                              int64_t dim0);
int futhark_free_i32_1d(struct futhark_context *ctx,
                        struct futhark_i32_1d *arr);
int futhark_values_i32_1d(struct futhark_context *ctx,
                          struct futhark_i32_1d *arr, int32_t *data);
cl_mem futhark_values_raw_i32_1d(struct futhark_context *ctx,
                                 struct futhark_i32_1d *arr);
int64_t *futhark_shape_i32_1d(struct futhark_context *ctx,
                              struct futhark_i32_1d *arr);
struct futhark_i8_1d ;
struct futhark_i8_1d *futhark_new_i8_1d(struct futhark_context *ctx,
                                        int8_t *data, int64_t dim0);
struct futhark_i8_1d *futhark_new_raw_i8_1d(struct futhark_context *ctx,
                                            cl_mem data, int offset,
                                            int64_t dim0);
int futhark_free_i8_1d(struct futhark_context *ctx, struct futhark_i8_1d *arr);
int futhark_values_i8_1d(struct futhark_context *ctx, struct futhark_i8_1d *arr,
                         int8_t *data);
cl_mem futhark_values_raw_i8_1d(struct futhark_context *ctx,
                                struct futhark_i8_1d *arr);
int64_t *futhark_shape_i8_1d(struct futhark_context *ctx,
                             struct futhark_i8_1d *arr);

/*
 * Opaque values
*/

struct futhark_opaque_arr_association_1d ;
int futhark_free_opaque_arr_association_1d(struct futhark_context *ctx,
                                           struct futhark_opaque_arr_association_1d *obj);
struct futhark_opaque_arr_dXdt_1d ;
int futhark_free_opaque_arr_dXdt_1d(struct futhark_context *ctx,
                                    struct futhark_opaque_arr_dXdt_1d *obj);
struct futhark_opaque_arr_forceTorque_1d ;
int futhark_free_opaque_arr_forceTorque_1d(struct futhark_context *ctx,
                                           struct futhark_opaque_arr_forceTorque_1d *obj);
struct futhark_opaque_arr_rigid_1d ;
int futhark_free_opaque_arr_rigid_1d(struct futhark_context *ctx,
                                     struct futhark_opaque_arr_rigid_1d *obj);
struct futhark_opaque_box ;
int futhark_free_opaque_box(struct futhark_context *ctx,
                            struct futhark_opaque_box *obj);

/*
 * Entry points
*/

int futhark_entry_rodNetworkPairInteraction(struct futhark_context *ctx,
                                            struct futhark_opaque_arr_forceTorque_1d **out0,
                                            const float in0, const float in1,
                                            const struct futhark_f32_2d *in2,
                                            const struct futhark_f32_3d *in3,
                                            const struct futhark_f32_2d *in4,
                                            const
                                            struct futhark_opaque_box *in5,
                                            const
                                            struct futhark_opaque_arr_rigid_1d *in6,
                                            const
                                            struct futhark_opaque_arr_association_1d *in7);
int futhark_entry_asgd(struct futhark_context *ctx, float *out0,
                       struct futhark_f32_2d **out1,
                       struct futhark_f32_3d **out2,
                       struct futhark_f32_2d **out3, const float in0, const
                       float in1, const float in2, const float in3, const
                       float in4, const int32_t in5, const int32_t in6, const
                       struct futhark_f32_2d *in7, const
                       struct futhark_f32_3d *in8, const
                       struct futhark_f32_2d *in9, const
                       struct futhark_f32_2d *in10, const
                       struct futhark_f32_2d *in11, const int32_t in12);
int futhark_entry_testNet(struct futhark_context *ctx, float *out0, const
                          struct futhark_f32_2d *in0, const
                          struct futhark_f32_3d *in1, const
                          struct futhark_f32_2d *in2, const
                          struct futhark_f32_2d *in3, const
                          struct futhark_f32_2d *in4);
int futhark_entry_bulkEval(struct futhark_context *ctx,
                           struct futhark_f32_2d **out0, const
                           struct futhark_f32_2d *in0, const
                           struct futhark_f32_3d *in1, const
                           struct futhark_f32_2d *in2, const
                           struct futhark_f32_2d *in3);
int futhark_entry_sgd(struct futhark_context *ctx, struct futhark_f32_2d **out0,
                      struct futhark_f32_3d **out1,
                      struct futhark_f32_2d **out2, const float in0, const
                      float in1, const int32_t in2, const int32_t in3, const
                      struct futhark_f32_2d *in4, const
                      struct futhark_f32_3d *in5, const
                      struct futhark_f32_2d *in6, const
                      struct futhark_f32_2d *in7, const
                      struct futhark_f32_2d *in8, const int32_t in9);
int futhark_entry_gradientDescent(struct futhark_context *ctx, float *out0,
                                  struct futhark_f32_2d **out1,
                                  struct futhark_f32_3d **out2,
                                  struct futhark_f32_2d **out3, const float in0,
                                  const struct futhark_f32_2d *in1, const
                                  struct futhark_f32_3d *in2, const
                                  struct futhark_f32_2d *in3, const
                                  struct futhark_f32_2d *in4, const
                                  struct futhark_f32_2d *in5, const
                                  int32_t in6);
int futhark_entry_dXdtsAndUpdatedSystem(struct futhark_context *ctx,
                                        struct futhark_opaque_arr_dXdt_1d **out0,
                                        struct futhark_opaque_arr_rigid_1d **out1,
                                        const float in0, const
                                        struct futhark_opaque_box *in1, const
                                        struct futhark_opaque_arr_rigid_1d *in2,
                                        const
                                        struct futhark_opaque_arr_forceTorque_1d *in3);
int futhark_entry_dXdts(struct futhark_context *ctx,
                        struct futhark_opaque_arr_dXdt_1d **out0, const
                        struct futhark_opaque_arr_rigid_1d *in0, const
                        struct futhark_opaque_arr_forceTorque_1d *in1);
int futhark_entry_rk4weightedUpdate(struct futhark_context *ctx,
                                    struct futhark_opaque_arr_rigid_1d **out0,
                                    const float in0, const
                                    struct futhark_opaque_box *in1, const
                                    struct futhark_opaque_arr_rigid_1d *in2,
                                    const
                                    struct futhark_opaque_arr_dXdt_1d *in3,
                                    const
                                    struct futhark_opaque_arr_dXdt_1d *in4,
                                    const
                                    struct futhark_opaque_arr_dXdt_1d *in5,
                                    const
                                    struct futhark_opaque_arr_dXdt_1d *in6);
int futhark_entry_coordinatesFrom(struct futhark_context *ctx,
                                  struct futhark_f32_1d **out0,
                                  struct futhark_f32_1d **out1,
                                  struct futhark_f32_1d **out2,
                                  struct futhark_f32_1d **out3,
                                  struct futhark_f32_1d **out4,
                                  struct futhark_f32_1d **out5,
                                  struct futhark_f32_1d **out6,
                                  struct futhark_f32_1d **out7,
                                  struct futhark_f32_1d **out8,
                                  struct futhark_f32_1d **out9,
                                  struct futhark_f32_1d **out10,
                                  struct futhark_f32_1d **out11,
                                  struct futhark_f32_1d **out12, const
                                  struct futhark_opaque_arr_rigid_1d *in0);
int futhark_entry_toAssociations(struct futhark_context *ctx,
                                 struct futhark_opaque_arr_association_1d **out0,
                                 const struct futhark_i32_1d *in0, const
                                 struct futhark_i32_1d *in1, const
                                 struct futhark_i8_1d *in2, const
                                 struct futhark_i8_1d *in3, const
                                 struct futhark_i8_1d *in4);
int futhark_entry_toRigids(struct futhark_context *ctx,
                           struct futhark_opaque_arr_rigid_1d **out0, const
                           struct futhark_f32_1d *in0, const
                           struct futhark_f32_1d *in1, const
                           struct futhark_f32_1d *in2, const
                           struct futhark_f32_1d *in3, const
                           struct futhark_f32_1d *in4, const
                           struct futhark_f32_1d *in5, const
                           struct futhark_f32_1d *in6, const
                           struct futhark_f32_1d *in7, const
                           struct futhark_f32_1d *in8, const
                           struct futhark_f32_1d *in9, const
                           struct futhark_f32_1d *in10, const
                           struct futhark_f32_1d *in11, const
                           struct futhark_f32_1d *in12, const
                           struct futhark_f32_1d *in13, const
                           struct futhark_f32_1d *in14, const
                           struct futhark_f32_1d *in15, const
                           struct futhark_f32_1d *in16, const
                           struct futhark_i32_1d *in17);
int futhark_entry_addForceTorques(struct futhark_context *ctx,
                                  struct futhark_opaque_arr_forceTorque_1d **out0,
                                  const
                                  struct futhark_opaque_arr_forceTorque_1d *in0,
                                  const
                                  struct futhark_opaque_arr_forceTorque_1d *in1);
int futhark_entry_toForceTorques(struct futhark_context *ctx,
                                 struct futhark_opaque_arr_forceTorque_1d **out0,
                                 const struct futhark_f32_1d *in0, const
                                 struct futhark_f32_1d *in1, const
                                 struct futhark_f32_1d *in2, const
                                 struct futhark_f32_1d *in3, const
                                 struct futhark_f32_1d *in4, const
                                 struct futhark_f32_1d *in5);
int futhark_entry_box(struct futhark_context *ctx,
                      struct futhark_opaque_box **out0, const float in0, const
                      float in1, const float in2, const float in3, const
                      float in4, const float in5, const bool in6, const
                      bool in7, const bool in8);

/*
 * Miscellaneous
*/

void futhark_debugging_report(struct futhark_context *ctx);
