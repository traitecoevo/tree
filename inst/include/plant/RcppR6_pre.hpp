// Generated by RcppR6 (0.2.3): do not edit by hand
#ifndef _PLANT_RCPPR6_PRE_HPP_
#define _PLANT_RCPPR6_PRE_HPP_

#include <RcppCommon.h>


namespace plant {
namespace RcppR6 {
template <typename T> class RcppR6;
}
}

namespace plant { namespace ode { namespace test { class OdeR; } } }

namespace Rcpp {
template <typename T> SEXP wrap(const plant::RcppR6::RcppR6<T>&);
namespace traits {
template <typename T> class Exporter<plant::RcppR6::RcppR6<T> >;
}

template <> SEXP wrap(const plant::ode::test::Lorenz&);
template <> plant::ode::test::Lorenz as(SEXP);
template <> SEXP wrap(const plant::ode::test::OdeR&);
template <> plant::ode::test::OdeR as(SEXP);
template <> SEXP wrap(const plant::ode::Runner<plant::ode::test::Lorenz>&);
template <> plant::ode::Runner<plant::ode::test::Lorenz> as(SEXP);

template <> SEXP wrap(const plant::ode::Runner<plant::ode::test::OdeR>&);
template <> plant::ode::Runner<plant::ode::test::OdeR> as(SEXP);

template <> SEXP wrap(const plant::ode::Runner<plant::tools::PlantRunner>&);
template <> plant::ode::Runner<plant::tools::PlantRunner> as(SEXP);
template <> SEXP wrap(const plant::CohortScheduleEvent&);
template <> plant::CohortScheduleEvent as(SEXP);
template <> SEXP wrap(const plant::CohortSchedule&);
template <> plant::CohortSchedule as(SEXP);
template <> SEXP wrap(const plant::Disturbance&);
template <> plant::Disturbance as(SEXP);
template <> SEXP wrap(const plant::Control&);
template <> plant::Control as(SEXP);
template <> SEXP wrap(const plant::ode::OdeControl&);
template <> plant::ode::OdeControl as(SEXP);
template <> SEXP wrap(const plant::quadrature::QK&);
template <> plant::quadrature::QK as(SEXP);
template <> SEXP wrap(const plant::quadrature::QAG&);
template <> plant::quadrature::QAG as(SEXP);
template <> SEXP wrap(const plant::interpolator::Interpolator&);
template <> plant::interpolator::Interpolator as(SEXP);
template <> SEXP wrap(const plant::Environment&);
template <> plant::Environment as(SEXP);
template <> SEXP wrap(const plant::Plant_internals&);
template <> plant::Plant_internals as(SEXP);
template <> SEXP wrap(const plant::Plant<plant::FFW16_Strategy>&);
template <> plant::Plant<plant::FFW16_Strategy> as(SEXP);
template <> SEXP wrap(const plant::tools::PlantRunner&);
template <> plant::tools::PlantRunner as(SEXP);
template <> SEXP wrap(const plant::FFW16_Strategy&);
template <> plant::FFW16_Strategy as(SEXP);
template <> SEXP wrap(const plant::FFW16_Parameters&);
template <> plant::FFW16_Parameters as(SEXP);
template <> SEXP wrap(const plant::FFW16_PlantPlus&);
template <> plant::FFW16_PlantPlus as(SEXP);
template <> SEXP wrap(const plant::FFW16_PlantPlus::internals&);
template <> plant::FFW16_PlantPlus::internals as(SEXP);
template <> SEXP wrap(const plant::Cohort<plant::FFW16_Plant>&);
template <> plant::Cohort<plant::FFW16_Plant> as(SEXP);
template <> SEXP wrap(const plant::Species<plant::FFW16_Plant>&);
template <> plant::Species<plant::FFW16_Plant> as(SEXP);
template <> SEXP wrap(const plant::Patch<plant::FFW16_Plant>&);
template <> plant::Patch<plant::FFW16_Plant> as(SEXP);
template <> SEXP wrap(const plant::EBT<plant::FFW16_Plant>&);
template <> plant::EBT<plant::FFW16_Plant> as(SEXP);
template <> SEXP wrap(const plant::StochasticSpecies<plant::FFW16_Plant>&);
template <> plant::StochasticSpecies<plant::FFW16_Plant> as(SEXP);
template <> SEXP wrap(const plant::StochasticPatch<plant::FFW16_Plant>&);
template <> plant::StochasticPatch<plant::FFW16_Plant> as(SEXP);
}

#endif
