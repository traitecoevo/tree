// Generated by RcppR6 (0.2.4): do not edit by hand
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
template <> SEXP wrap(const plant::Plant<plant::FF16_Strategy>&);
template <> plant::Plant<plant::FF16_Strategy> as(SEXP);

template <> SEXP wrap(const plant::Plant<plant::FFdev_Strategy>&);
template <> plant::Plant<plant::FFdev_Strategy> as(SEXP);
template <> SEXP wrap(const plant::tools::PlantRunner&);
template <> plant::tools::PlantRunner as(SEXP);
template <> SEXP wrap(const plant::FF16_Strategy&);
template <> plant::FF16_Strategy as(SEXP);
template <> SEXP wrap(const plant::FFdev_Strategy&);
template <> plant::FFdev_Strategy as(SEXP);
template <> SEXP wrap(const plant::Parameters<plant::FF16_Strategy>&);
template <> plant::Parameters<plant::FF16_Strategy> as(SEXP);

template <> SEXP wrap(const plant::Parameters<plant::FFdev_Strategy>&);
template <> plant::Parameters<plant::FFdev_Strategy> as(SEXP);
template <> SEXP wrap(const plant::PlantPlus<plant::FF16_Strategy>&);
template <> plant::PlantPlus<plant::FF16_Strategy> as(SEXP);

template <> SEXP wrap(const plant::PlantPlus<plant::FFdev_Strategy>&);
template <> plant::PlantPlus<plant::FFdev_Strategy> as(SEXP);
template <> SEXP wrap(const plant::PlantPlus_internals&);
template <> plant::PlantPlus_internals as(SEXP);
template <> SEXP wrap(const plant::Cohort<plant::FF16_Strategy>&);
template <> plant::Cohort<plant::FF16_Strategy> as(SEXP);

template <> SEXP wrap(const plant::Cohort<plant::FFdev_Strategy>&);
template <> plant::Cohort<plant::FFdev_Strategy> as(SEXP);
template <> SEXP wrap(const plant::Species<plant::FF16_Strategy>&);
template <> plant::Species<plant::FF16_Strategy> as(SEXP);

template <> SEXP wrap(const plant::Species<plant::FFdev_Strategy>&);
template <> plant::Species<plant::FFdev_Strategy> as(SEXP);
template <> SEXP wrap(const plant::Patch<plant::FF16_Strategy>&);
template <> plant::Patch<plant::FF16_Strategy> as(SEXP);

template <> SEXP wrap(const plant::Patch<plant::FFdev_Strategy>&);
template <> plant::Patch<plant::FFdev_Strategy> as(SEXP);
template <> SEXP wrap(const plant::EBT<plant::FF16_Strategy>&);
template <> plant::EBT<plant::FF16_Strategy> as(SEXP);

template <> SEXP wrap(const plant::EBT<plant::FFdev_Strategy>&);
template <> plant::EBT<plant::FFdev_Strategy> as(SEXP);
template <> SEXP wrap(const plant::StochasticSpecies<plant::FF16_Strategy>&);
template <> plant::StochasticSpecies<plant::FF16_Strategy> as(SEXP);

template <> SEXP wrap(const plant::StochasticSpecies<plant::FFdev_Strategy>&);
template <> plant::StochasticSpecies<plant::FFdev_Strategy> as(SEXP);
template <> SEXP wrap(const plant::StochasticPatch<plant::FF16_Strategy>&);
template <> plant::StochasticPatch<plant::FF16_Strategy> as(SEXP);

template <> SEXP wrap(const plant::StochasticPatch<plant::FFdev_Strategy>&);
template <> plant::StochasticPatch<plant::FFdev_Strategy> as(SEXP);
template <> SEXP wrap(const plant::StochasticPatchRunner<plant::FF16_Strategy>&);
template <> plant::StochasticPatchRunner<plant::FF16_Strategy> as(SEXP);

template <> SEXP wrap(const plant::StochasticPatchRunner<plant::FFdev_Strategy>&);
template <> plant::StochasticPatchRunner<plant::FFdev_Strategy> as(SEXP);
}

#endif
